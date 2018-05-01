{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
{-# OPTIONS -Wno-noncanonical-monoid-instances #-}
#endif

module Lightning.Internal
  ( -- * Data types
    Hints (..)
  , Reply (..)
  , Consumption (..)
  , Result (..)
  , Parser (..)
    -- * Primitive combintaors
  , failure
  , fancyFailure
  , label
  , hidden
  , try
  , lookAhead
  , notFollowedBy
  , withRecovery
  , observing
  , eof
  , token
  , tokens
  , takeWhileP
  , takeWhile1P
  , takeP
  , getParserState
  , updateParserState
    -- * Helper functions
  , toHints
  , withHints
  , accHints
  , refreshLastHint
  , getReply )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Set (Set)
import Data.String (IsString (..))
import Lightning.Error
import Lightning.Pos
import Lightning.State
import Lightning.Stream
import qualified Control.Monad.Fail as Fail
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

----------------------------------------------------------------------------
-- Data types

newtype Hints = Hints [Set ErrorItem]
  deriving (Semigroup, Monoid)

data Reply a = Reply State Consumption (Result a)

data Consumption
  = Consumed           -- ^ Some part of input stream was consumed
  | Virgin             -- ^ No input was consumed

data Result a
  = OK a               -- ^ Parser succeeded
  | Error ParseError   -- ^ Parser failed

newtype Parser a = Parser
  { unParser
      :: forall b. State
      -> (a -> State -> Hints -> b) -- consumed-OK
      -> (ParseError -> State -> b) -- consumed-error
      -> (a -> State -> Hints -> b) -- empty-OK
      -> (ParseError -> State -> b) -- empty-error
      -> b
  }

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}
  sconcat = fmap sconcat . sequence
  {-# INLINE sconcat #-}

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
#if MIN_VERSION_base(4,11,0)
  mappend = (<>)
#else
  mappend = liftA2 mappend
#endif
  {-# INLINE mappend #-}
  mconcat = fmap mconcat . sequence
  {-# INLINE mconcat #-}

-- instance (a ~ Tokens, IsString a, Eq a) => IsString (Parser a) where
--   fromString s = tokens (==) (fromString s)

instance Functor Parser where
  fmap = pMap

pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = Parser $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

instance Applicative Parser where
  pure     = pPure
  (<*>)    = pAp
  p1 *> p2 = p1 `pBind` const p2
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

pPure :: a -> Parser a
pPure x = Parser $ \s _ _ eok _ -> eok x s mempty
{-# INLINE pPure #-}

pAp :: Parser (a -> b) -> Parser a -> Parser b
pAp m k = Parser $ \s cok cerr eok eerr ->
  let mcok x s' hs = unParser k s' (cok . x) cerr
        (accHints hs (cok . x)) (withHints hs cerr)
      meok x s' hs = unParser k s' (cok . x) cerr
        (accHints hs (eok . x)) (withHints hs eerr)
  in unParser m s mcok cerr meok eerr
{-# INLINE pAp #-}

instance Alternative Parser where
  empty  = mzero
  (<|>)  = mplus

instance Monad Parser where
  return = pure
  (>>=)  = pBind
  fail   = Fail.fail

pBind :: Parser a -> (a -> Parser b) -> Parser b
pBind m k = Parser $ \s cok cerr eok eerr ->
  let mcok x s' hs = unParser (k x) s' cok cerr
        (accHints hs cok) (withHints hs cerr)
      meok x s' hs = unParser (k x) s' cok cerr
        (accHints hs eok) (withHints hs eerr)
  in unParser m s mcok cerr meok eerr
{-# INLINE pBind #-}

instance Fail.MonadFail Parser where
  fail = pFail

pFail :: String -> Parser a
pFail msg = Parser $ \s@State {..} _ _ _ eerr ->
  let d = E.singleton (ErrorFail msg)
  in eerr (FancyError stPos d) s
{-# INLINE pFail #-}

instance MonadPlus Parser where
  mzero = pZero
  mplus = pPlus

pZero :: Parser a
pZero = Parser $ \s@State {..} _ _ _ eerr ->
  eerr (TrivialError stPos Nothing E.empty) s
{-# INLINE pZero #-}

pPlus :: Parser a -> Parser a -> Parser a
pPlus m n = Parser $ \s cok cerr eok eerr ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s' hs  = eok x s' (toHints (statePos s') err <> hs)
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

instance MonadFix Parser where
  mfix f = mkPT $ \s -> mfix $ \(~(Reply _ _ result)) -> do
    let
      a = case result of
        OK a' -> a'
        Error _ -> error "mfix Parser"
    runParser (f a) s

mkPT :: (State -> Reply a) -> Parser a
mkPT k = Parser $ \s cok cerr eok eerr ->
  let (Reply s' consumption result) = k s
  in case consumption of
    Consumed ->
      case result of
        OK    x -> cok x s' mempty
        Error e -> cerr e s'
    Virgin ->
      case result of
        OK    x -> eok x s' mempty
        Error e -> eerr e s'

----------------------------------------------------------------------------
-- Primitive combinators

failure
  :: Maybe ErrorItem
  -> Set ErrorItem
  -> Parser a
failure us ps = Parser $ \s@State {..} _ _ _ eerr ->
  eerr (TrivialError stPos us ps) s
{-# INLINE failure #-}

fancyFailure
  :: Set ErrorFancy
  -> Parser a
fancyFailure xs = Parser $ \s@State {..} _ _ _ eerr ->
  eerr (FancyError stPos xs) s
{-# INLINE fancyFailure #-}

label :: String -> Parser a -> Parser a
label l p = Parser $ \s cok cerr eok eerr ->
  let el = Label <$> NE.nonEmpty l
      cl = Label . (NE.fromList "the rest of " <>) <$> NE.nonEmpty l
      cok' x s' hs = cok x s' (refreshLastHint hs cl)
      eok' x s' hs = eok x s' (refreshLastHint hs el)
      eerr'    err = eerr $
        case err of
          (TrivialError pos us _) ->
            TrivialError pos us (maybe E.empty E.singleton el)
          _ -> err
  in unParser p s cok' cerr eok' eerr'
{-# INLINE label #-}

try :: Parser a -> Parser a
try p = Parser $ \s cok _ eok eerr ->
  let eerr' err _ = eerr err s
  in unParser p s cok eerr' eok eerr'
{-# INLINE try #-}

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s _ cerr eok eerr ->
  let eok' a _ _ = eok a s mempty
  in unParser p s eok' cerr eok' eerr
{-# INLINE lookAhead #-}

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \s@State {..} _ _ eok eerr ->
  let what = maybe EndOfInput (Tokens . nes . fst) (take1_ input)
      unexpect u = TrivialError stPos (pure u) E.empty
      cok' _ _ _ = eerr (unexpect what) s
      cerr'  _ _ = eok () s mempty
      eok' _ _ _ = eerr (unexpect what) s
      eerr'  _ _ = eok () s mempty
  in unParser p s cok' cerr' eok' eerr'
{-# INLINE notFollowedBy #-}

withRecovery
  :: (ParseError -> Parser a)
  -> Parser a
  -> Parser a
withRecovery r p = Parser $ \s cok cerr eok eerr ->
  let mcerr err ms =
        let rcok x s' _ = cok x s' mempty
            rcerr   _ _ = cerr err ms
            reok x s' _ = eok x s' (toHints (statePos s') err)
            reerr   _ _ = cerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
      meerr err ms =
        let rcok x s' _ = cok x s' (toHints (statePos s') err)
            rcerr   _ _ = eerr err ms
            reok x s' _ = eok x s' (toHints (statePos s') err)
            reerr   _ _ = eerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
  in unParser p s cok mcerr eok meerr
{-# INLINE withRecovery #-}

observing
  :: Parser a
  -> Parser (Either ParseError a)
observing p = Parser $ \s cok _ eok _ ->
  let cerr' err s' = cok (Left err) s' mempty
      eerr' err s' = eok (Left err) s' (toHints (statePos s') err)
  in unParser p s (cok . Right) cerr' (eok . Right) eerr'
{-# INLINE observing #-}

eof :: Parser ()
eof = Parser $ \s@(State input pos tp w) _ _ eok eerr ->
  case take1_ input of
    Nothing    -> eok () s mempty
    Just (x,_) ->
      let !apos = positionAt1 pos x
          us    = (pure . Tokens . nes) x
          ps    = E.singleton EndOfInput
      in eerr (TrivialError apos us ps)
          (State input apos tp w)
{-# INLINE eof #-}

token
  :: (Token -> Maybe a)
  -> Set ErrorItem
  -> Parser a
token test ps = Parser $ \s@(State input pos tp w) cok _ _ eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
      in eerr (TrivialError pos us ps) s
    Just (c,cs) ->
      case test c of
        Nothing ->
          let !apos = positionAt1 pos c
              us    = (Just . Tokens . nes) c
          in eerr (TrivialError apos us ps)
                  (State input apos tp w)
        Just x ->
          let !npos = advance1 w pos c
              newstate = State cs npos (tp + 1) w
          in cok x newstate mempty
{-# INLINE token #-}

tokens
  :: (Tokens -> Tokens -> Bool)
  -> Tokens
  -> Parser Tokens
tokens f tts = Parser $ \s@(State input pos tp w) cok _ eok eerr ->
  let unexpect pos' u =
        let us = pure u
            ps = (E.singleton . Tokens . NE.fromList . chunkToTokens pxy) tts
        in TrivialError pos' us ps
      len = chunkLength pxy tts
  in case takeN_ len input of
    Nothing ->
      eerr (unexpect pos EndOfInput) s
    Just (tts', input') ->
      if f tts tts'
        then let !npos = advanceN pxy w pos tts'
                 st    = State input' npos (tp + len) w
             in if chunkEmpty pxy tts
                  then eok tts' st mempty
                  else cok tts' st mempty
        else let !apos = positionAtN pxy pos tts'
                 ps = (Tokens . NE.fromList . chunkToTokens pxy) tts'
             in eerr (unexpect apos ps) (State input apos tp w)
{-# INLINE tokens #-}

takeWhileP
  :: Maybe String
  -> (Token -> Bool)
  -> Parser Tokens
takeWhileP ml f = Parser $ \(State input pos tp w) cok _ eok _ ->
  let (ts, input') = takeWhile_ f input
      !npos = advanceN pxy w pos ts
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
  in if chunkEmpty pxy ts
       then eok ts (State input' npos (tp + len) w) hs
       else cok ts (State input' npos (tp + len) w) hs
{-# INLINE takeWhileP #-}

takeWhile1P
  :: Maybe String
  -> (Token -> Bool)
  -> Parser Tokens
takeWhile1P ml f = Parser $ \State {..} cok _ _ eerr ->
  let (ts, input') = takeWhile_ f stInput
      len = chunkLength ts
      el = Label <$> (ml >>= NE.nonEmpty)
      hs =
        case el of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton) l
  in if chunkEmpty ts
       then let !apos = positionAtN stPos ts
                us    = pure $
                  case take1_ stInput of
                    Nothing -> EndOfInput
                    Just (t,_) -> Tokens (nes t)
                ps    = maybe E.empty E.singleton el
            in eerr (TrivialError apos us ps)
                    State { stPos = apos, .. }
       else let !npos = advanceN stTabWidth stPos ts
            in cok ts State
                   { stInput = input'
                   , stPos   = npos
                   , stTP    = stTP + len
                   , .. }
                   hs
{-# INLINE takeWhile1P #-}

takeP
  :: Maybe String
  -> Int
  -> Parser Tokens
takeP ml n = Parser $ \s@State {..} cok _ _ eerr ->
  let el = Label <$> (ml >>= NE.nonEmpty)
      ps = maybe E.empty E.singleton el
  in case takeN_ n stInput of
       Nothing ->
         eerr (TrivialError stPos (pure EndOfInput) ps) s
       Just (ts, input') ->
         let len   = chunkLength ts
             !apos = positionAtN stPos ts
             !npos = advanceN stTabWidth stPos ts
         in if len /= n
           then eerr (TrivialError npos (pure EndOfInput) ps)
                     State { stPos = apos, .. }
           else cok ts State
                  { stInput = input'
                  , stPos   = npos
                  , stTP    = stTP + len
                  , .. }
                  mempty
{-# INLINE takeP #-}

getParserState :: Parser State
getParserState = Parser $ \s _ _ eok _ -> eok s s mempty
{-# INLINE getParserState #-}

updateParserState :: (State -> State) -> Parser ()
updateParserState f = Parser $ \s _ _ eok _ -> eok () (f s) mempty
{-# INLINE updateParserState #-}

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

----------------------------------------------------------------------------
-- Helper functions

-- | Convert 'ParseError' record into 'Hints'.

toHints
  :: SourcePos         -- ^ Current position in input stream
  -> ParseError        -- ^ Parse error to convert
  -> Hints
toHints streamPos = \case
  TrivialError errPos _ ps ->
    -- NOTE This is important to check here that the error indeed has
    -- happened at the same position as current position of stream because
    -- there might have been backtracking with 'try' and in that case we
    -- must not convert such a parse error to hints.
    if streamPos == errPos
      then Hints (if E.null ps then [] else [ps])
      else mempty
  FancyError _ _ -> mempty
{-# INLINE toHints #-}

-- | @'withHints' hs c@ makes “error” continuation @c@ use given hints @hs@.
--
-- Note that if resulting continuation gets 'ParseError' that has custom
-- data in it, hints are ignored.

withHints
  :: Hints             -- ^ Hints to use
  -> (ParseError -> State -> b) -- ^ Continuation to influence
  -> ParseError        -- ^ First argument of resulting continuation
  -> State             -- ^ Second argument of resulting continuation
  -> b
withHints (Hints ps') c e =
  case e of
    TrivialError pos us ps -> c (TrivialError pos us (E.unions (ps : ps')))
    _ -> c e
{-# INLINE withHints #-}

-- | @'accHints' hs c@ results in “OK” continuation that will add given
-- hints @hs@ to third argument of original continuation @c@.

accHints
  :: Hints             -- ^ 'Hints' to add
  -> (a -> State -> Hints -> b) -- ^ An “OK” continuation to alter
  -> (a -> State -> Hints -> b) -- ^ Altered “OK” continuation
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)
{-# INLINE accHints #-}

-- | Replace the most recent group of hints (if any) with the given
-- 'ErrorItem' (or delete it if 'Nothing' is given). This is used in 'label'
-- primitive.

refreshLastHint :: Hints -> Maybe ErrorItem -> Hints
refreshLastHint (Hints [])     _        = Hints []
refreshLastHint (Hints (_:xs)) Nothing  = Hints xs
refreshLastHint (Hints (_:xs)) (Just m) = Hints (E.singleton m : xs)
{-# INLINE refreshLastHint #-}

-- | Low-level unpacking of the 'Parser' type.

runParser
  :: Parser a          -- ^ Parser to run
  -> State             -- ^ Initial state
  -> Reply a
runParser p s = unParser p s cok cerr eok eerr
  where
    cok a s' _  = Reply s' Consumed (OK a)
    cerr err s' = Reply s' Consumed (Error err)
    eok a s' _  = Reply s' Virgin   (OK a)
    eerr err s' = Reply s' Virgin   (Error err)
