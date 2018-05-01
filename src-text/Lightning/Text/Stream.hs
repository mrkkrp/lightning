module Lightning.Text.Stream
  ( Stream
  , Token
  , Tokens
  , tokenToChunk
  , tokensToChunk
  , chunkToTokens
  , chunkLength
  , chunkEmpty
  , positionAt1
  , positionAtN
  , advance1
  , advanceN
  , take1_
  , takeN_
  , takeWhile_
  )
where

import Data.Text (Text)
import Text.Pos
import qualified Data.Text as T

type Stream = Text

type Token = Char

type Tokens = Text

tokenToChunk :: Token -> Tokens
tokenToChunk = T.singleton
{-# INLINE tokenToChunk #-}

tokensToChunk :: [Token] -> Tokens
tokensToChunk = T.pack
{-# INLINE tokensToChunk #-}

chunkToTokens :: Tokens -> [Token]
chunkToTokens = T.unpack
{-# INLINE chunkToTokens #-}

chunkLength ::Tokens -> Int
chunkLength = T.length
{-# INLINE chunkLength #-}

chunkEmpty :: Tokens -> Bool
chunkEmpty = T.null
{-# INLINE chunkEmpty #-}

positionAt1
  :: SourcePos       -- ^ Current position
  -> Token           -- ^ Current token
  -> SourcePos       -- ^ Position of the token
positionAt1 = defaultPositionAt
{-# INLINE positionAt1 #-}

positionAtN
  :: SourcePos       -- ^ Current position
  -> Tokens          -- ^ Current chunk
  -> SourcePos       -- ^ Position of the chunk
positionAtN = defaultPositionAt
{-# INLINE positionAtN #-}

advance1 :: Pos -> SourcePos -> Token -> SourcePos
advance1 = defaultAdvance1
{-# INLINE advance1 #-}

advanceN :: Pos -> SourcePos -> Tokens -> SourcePos
advanceN w = T.foldl' (defaultAdvance1 w)
{-# INLINE advanceN #-}

take1_ :: Stream -> Maybe (Token, Stream)
take1_ = T.uncons
{-# INLINE take1_ #-}

takeN_ :: Int -> Stream -> Maybe (Tokens, Stream)
takeN_ n s
  | n <= 0    = Just (T.empty, s)
  | T.null s  = Nothing
  | otherwise = Just (T.splitAt n s)
{-# INLINE takeN_ #-}

takeWhile_ :: (Token -> Bool) -> Stream -> (Tokens, Stream)
takeWhile_ = T.span
{-# INLINE takeWhile_ #-}

----------------------------------------------------------------------------
--

defaultPositionAt :: SourcePos -> a -> SourcePos
defaultPositionAt pos _ = pos
{-# INLINE defaultPositionAt #-}

defaultAdvance1
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> Char              -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 width (SourcePos n l c) t = npos
  where
    w  = unPos width
    c' = unPos c
    npos =
      case t of
        '\n' -> SourcePos n (l <> pos1) pos1
        '\t' -> SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
        _    -> SourcePos n l (c <> pos1)
{-# INLINE defaultAdvance1 #-}
