{-# LANGUAGE RecordWildCards #-}

module Lightning
  ( -- * Re-exports
    module Lightning.Pos
  , module Lightning.Error
  , module Control.Monad.Combinators
    -- * Data types
  , State (..)
  , Parser
    -- * Running parser
  , parse
  , parse'
  , parseMaybe
  , parseTest
  , parseTest'
    -- * Primitive combinators
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
    -- * Derivatives of primitive combinators
  , single
  , satisfy
  , anySingle
  , anySingleBut
  , oneOf
  , noneOf
  , chunk
  , (<?>)
  , unexpected
  , customFailure
  , match
  , region
  , takeRest
  , atEnd
    -- * Parser state combinators
  , getInput
  , setInput
  , getPosition
  , getNextTokenPosition
  , setPosition
  , getTokensProcessed
  , setTokensProcessed
  , getTabWidth
  , setTabWidth
  , setParserState )
where

import Control.Monad.Combinators
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Lightning.Error
import Lightning.Error.Custom
import Lightning.Internal
import Lightning.Pos
import Lightning.State
import Lightning.State.Custom
import Lightning.Stream
import qualified Data.Set as E

----------------------------------------------------------------------------
-- Data types

----------------------------------------------------------------------------
-- Running a parser

parse
  :: Parser a          -- ^ Parser to run
  -> String            -- ^ Name of source file
  -> Stream            -- ^ Input for parser
  -> Either ParseError a
parse p name s = snd $ parse' p (initialState name s)

parse'
  :: Parser a          -- ^ Parser to run
  -> State             -- ^ Input for parser
  -> (State, Either ParseError a)
parse' p s =
  let Reply s' _ result = getReply p s
      r = case result of
            OK    x -> Right x
            Error e -> Left e
  in (s', r)

parseMaybe :: Parser a -> Stream -> Maybe a
parseMaybe p s =
  case parse (p <* eof) "" s of
    Left  _ -> Nothing
    Right x -> Just x

parseTest :: Show a
  => Parser a          -- ^ Parser to run
  -> Stream            -- ^ Input for parser
  -> IO ()
parseTest p input =
  case parse p "" input of
    Left  e -> putStr (parseErrorPretty e)
    Right x -> print x

parseTest' :: Show a
  => Parser a          -- ^ Parser to run
  -> Stream            -- ^ Input for parser
  -> IO ()
parseTest' p input =
  case parse p "" input of
    Left  e -> putStr (parseErrorPretty' input e)
    Right x -> print x

-- | Given name of source file and input construct initial state for parser.

initialState :: String -> Stream -> State
initialState name s = State
  { stInput    = s
  , stPos      = initialPos name
  , stTP       = 0
  , stTabWidth = defaultTabWidth
  , stCustom   = initialCustomState
  }

----------------------------------------------------------------------------
-- Derivatives of primitive combinators

-- | @'single' t@ only matches the single token @t@.
--
-- > semicolon = single ';'
--
-- See also: 'token', 'anySingle', 'Text.Megaparsec.Byte.char',
-- 'Text.Megaparsec.Char.char'.

single
  :: Token             -- ^ Token to match
  -> Parser Token
single t = token testToken expected
  where
    testToken x = if x == t then Just x else Nothing
    expected    = E.singleton (Tokens (t:|[]))
{-# INLINE single #-}

-- | The parser @'satisfy' f@ succeeds for any token for which the supplied
-- function @f@ returns 'True'. Returns the character that is actually
-- parsed.
--
-- > digitChar = satisfy isDigit <?> "digit"
-- > oneOf cs  = satisfy (`elem` cs)
--
-- See also: 'anySingle', 'anySingleBut', 'oneOf', 'noneOf'.

satisfy
  :: (Token -> Bool)   -- ^ Predicate to apply
  -> Parser Token
satisfy f = token testChar E.empty
  where
    testChar x = if f x then Just x else Nothing
{-# INLINE satisfy #-}

-- | Parse and return a single token. It's a good idea to attach a 'label'
-- to this parser manually.
--
-- > anySingle = satisfy (const True)
--
-- See also: 'satisfy', 'anySingleBut'.

anySingle :: Parser Token
anySingle = satisfy (const True)
{-# INLINE anySingle #-}

-- | Match any token but the given one. It's a good idea to attach a 'label'
-- to this parser manually.
--
-- > anySingleBut t = satisfy (/= t)
--
-- See also: 'single', 'anySingle', 'satisfy'.

anySingleBut
  :: Token             -- ^ Token we should not match
  -> Parser Token
anySingleBut t = satisfy (/= t)
{-# INLINE anySingleBut #-}

-- | @'oneOf' ts@ succeeds if the current token is in the supplied
-- collection of tokens @ts@. Returns the parsed token. Note that this
-- parser cannot automatically generate the “expected” component of error
-- message, so usually you should label it manually with 'label' or ('<?>').
--
-- > oneOf cs = satisfy (`elem` cs)
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"
--
-- __Performance note__: prefer 'satisfy' when you can because it's faster
-- when you have only a couple of tokens to compare to:
--
-- > quoteFast = satisfy (\x -> x == '\'' || x == '\"')
-- > quoteSlow = oneOf "'\""

oneOf :: Foldable f
  => f Token           -- ^ Collection of matching tokens
  -> Parser Token
oneOf cs = satisfy (`elem` cs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @'noneOf' ts@ succeeds if the current token
-- /not/ in the supplied list of tokens @ts@. Returns the parsed character.
-- Note that this parser cannot automatically generate the “expected”
-- component of error message, so usually you should label it manually with
-- 'label' or ('<?>').
--
-- > noneOf cs = satisfy (`notElem` cs)
--
-- See also: 'satisfy'.
--
-- __Performance note__: prefer 'satisfy' and 'singleBut' when you can
-- because it's faster.

noneOf :: Foldable f
  => f Token           -- ^ Collection of taken we should not match
  -> Parser Token
noneOf cs = satisfy (`notElem` cs)
{-# INLINE noneOf #-}

-- | @'chunk' chk@ only matches the chunk @chk@.
--
-- > divOrMod = chunk "div" <|> chunk "mod"
--
-- See also: 'tokens', 'Text.Megaparsec.Char.string',
-- 'Text.Megaparsec.Byte.string'.

chunk
  :: Tokens            -- ^ Chunk to match
  -> Parser Tokens
chunk = tokens (==)
{-# INLINE chunk #-}

-- | A synonym for 'label' in the form of an operator.

infix 0 <?>

(<?>) :: Parser a -> String -> Parser a
(<?>) = flip label
{-# INLINE (<?>) #-}

-- | The parser @'unexpected' item@ fails with an error message telling
-- about unexpected item @item@ without consuming any input.
--
-- > unexpected item = failure (Just item) Set.empty

unexpected :: ErrorItem -> Parser a
unexpected item = failure (Just item) E.empty
{-# INLINE unexpected #-}

-- | Report a custom parse error. For a more general version, see
-- 'fancyFailure'.

customFailure :: CustomError -> Parser a
customFailure = fancyFailure . E.singleton . ErrorCustom
{-# INLINE customFailure #-}

-- | Return both the result of a parse and a chunk of input that was
-- consumed during parsing. This relies on the change of the
-- 'stateTokensProcessed' value to evaluate how many tokens were consumed.
-- If you mess with it manually in the argument parser, prepare for
-- troubles.

match :: Parser a -> Parser (Tokens, a)
match p = do
  tp  <- getTokensProcessed
  s   <- getInput
  r   <- p
  tp' <- getTokensProcessed
  -- NOTE The 'fromJust' call here should never fail because if the stream
  -- is empty before 'p' (the only case when 'takeN_' can return 'Nothing'
  -- as per its invariants), (tp' - tp) won't be greater than 0, and in that
  -- case 'Just' is guaranteed to be returned as per another invariant of
  -- 'takeN_'.
  return ((fst . fromJust) (takeN_ (tp' - tp) s), r)
{-# INLINEABLE match #-}

-- | Specify how to process 'ParseError's that happen inside of this
-- wrapper. As a side effect of the current implementation changing
-- 'errorPos' with this combinator will also change the final 'statePos' in
-- the parser state (try to avoid that because 'statePos' will go out of
-- sync with factual position in the input stream, which is probably OK if
-- you finish parsing right after that, but be warned).

region
  :: (ParseError -> ParseError)
     -- ^ How to process 'ParseError's
  -> Parser a          -- ^ The “region” that the processing applies to
  -> Parser a
region f m = do
  r <- observing m
  case r of
    Left err ->
      case f err of
        TrivialError pos us ps -> do
          updateParserState $ \st -> st { stPos = pos }
          failure us ps
        FancyError pos xs -> do
          updateParserState $ \st -> st { stPos = pos }
          fancyFailure xs
    Right x -> return x
{-# INLINEABLE region #-}

-- | Consume the rest of the input and return it as a chunk. This parser
-- never fails, but may return an empty chunk.
--
-- > takeRest = takeWhileP Nothing (const True)

takeRest :: Parser Tokens
takeRest = takeWhileP Nothing (const True)
{-# INLINE takeRest #-}

-- | Return 'True' when end of input has been reached.
--
-- > atEnd = option False (True <$ hidden eof)

atEnd :: Parser Bool
atEnd = option False (True <$ hidden eof)
{-# INLINE atEnd #-}

----------------------------------------------------------------------------
-- Parser state combinators

-- | Return the current input.

getInput :: Parser Stream
getInput = stInput <$> getParserState
{-# INLINE getInput #-}

-- | @'setInput' input@ continues parsing with @input@.

setInput :: Stream -> Parser ()
setInput input = updateParserState $ \s ->
  s { stInput = input }
{-# INLINE setInput #-}

-- | Return the current source position.
--
-- See also: 'getNextTokenPosition'.

getPosition :: Parser SourcePos
getPosition = stPos <$> getParserState
{-# INLINE getPosition #-}

-- | Get the position where the next token in the stream begins. If the
-- stream is empty, return 'Nothing'.
--
-- See also: 'getPosition'.

getNextTokenPosition :: Parser (Maybe SourcePos)
getNextTokenPosition = do
  State {..} <- getParserState
  let f = positionAt1 stPos
  return (f . fst <$> take1_ stInput)
{-# INLINEABLE getNextTokenPosition #-}

-- | @'setPosition' pos@ sets the current source position to @pos@.
--
-- See also: 'getPosition', 'pushPosition', 'popPosition', and 'SourcePos'.

setPosition :: SourcePos -> Parser ()
setPosition pos = updateParserState $ \s ->
  s { stPos = pos }
{-# INLINE setPosition #-}

-- | Get the number of tokens processed so far.
--
-- See also: 'setTokensProcessed'.

getTokensProcessed :: Parser Int
getTokensProcessed = stTP <$> getParserState
{-# INLINE getTokensProcessed #-}

-- | Set the number of tokens processed so far.
--
-- See also: 'getTokensProcessed'.

setTokensProcessed :: Int -> Parser ()
setTokensProcessed tp = updateParserState $ \s ->
  s { stTP = tp }
{-# INLINE setTokensProcessed #-}

-- | Return the tab width. The default tab width is equal to
-- 'defaultTabWidth'. You can set a different tab width with the help of
-- 'setTabWidth'.
--
-- See also: 'setTabWidth'.

getTabWidth :: Parser Pos
getTabWidth = stTabWidth <$> getParserState
{-# INLINE getTabWidth #-}

-- | Set tab width. If the argument of the function is not a positive
-- number, 'defaultTabWidth' will be used.
--
-- See also: 'getTabWidth'.

setTabWidth :: Pos -> Parser ()
setTabWidth w = updateParserState $ \s ->
  s { stTabWidth = w }
{-# INLINE setTabWidth #-}

-- | @'setParserState' st@ sets the parser state to @st@.
--
-- See also: 'getParserState', 'updateParserState'.

setParserState :: State -> Parser ()
setParserState st = updateParserState (const st)
{-# INLINE setParserState #-}
