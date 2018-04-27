{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
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
    -- * Helper functions
  , toHints
  , withHints
  , accHints
  , refreshLastHint
  , getReply )
where

import Lightning.Error
import Lightning.State
import Lightning.Stream

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

-- instance (a ~ Tokens, IsString a, Eq a, Stream s, Ord e)
--     => IsString (Parser e s m a) where
--   fromString s = tokens (==) (fromString s)

instance Functor Parser where
  fmap = pMap

pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

-- instance Stream s => Applicative (ParsecT e s m) where
--   pure     = pPure
--   (<*>)    = pAp
--   p1 *> p2 = p1 `pBind` const p2
--   p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

-- pPure :: a -> ParsecT e s m a
-- pPure x = ParsecT $ \s _ _ eok _ -> eok x s mempty
-- {-# INLINE pPure #-}

-- pAp :: Stream s
--   => ParsecT e s m (a -> b)
--   -> ParsecT e s m a
--   -> ParsecT e s m b
-- pAp m k = ParsecT $ \s cok cerr eok eerr ->
--   let mcok x s' hs = unParser k s' (cok . x) cerr
--         (accHints hs (cok . x)) (withHints hs cerr)
--       meok x s' hs = unParser k s' (cok . x) cerr
--         (accHints hs (eok . x)) (withHints hs eerr)
--   in unParser m s mcok cerr meok eerr
-- {-# INLINE pAp #-}
