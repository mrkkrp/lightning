module Lightning.Stream.ByteString
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
  , takeWhile_ )
where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import Lightning.Error.Class ()
import Lightning.Pos
import qualified Data.ByteString as B

type Stream = ByteString

type Token = Word8

type Tokens = ByteString

tokenToChunk :: Token -> Tokens
tokenToChunk = B.singleton
{-# INLINE tokenToChunk #-}

tokensToChunk :: [Token] -> Tokens
tokensToChunk = B.pack
{-# INLINE tokensToChunk #-}

chunkToTokens :: Tokens -> [Token]
chunkToTokens = B.unpack
{-# INLINE chunkToTokens #-}

chunkLength ::Tokens -> Int
chunkLength = B.length
{-# INLINE chunkLength #-}

chunkEmpty :: Tokens -> Bool
chunkEmpty = B.null
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
advanceN w = B.foldl' (defaultAdvance1 w)
{-# INLINE advanceN #-}

take1_ :: Stream -> Maybe (Token, Stream)
take1_ = B.uncons
{-# INLINE take1_ #-}

takeN_ :: Int -> Stream -> Maybe (Tokens, Stream)
takeN_ n s
  | n <= 0    = Just (B.empty, s)
  | B.null s  = Nothing
  | otherwise = Just (B.splitAt n s)
{-# INLINE takeN_ #-}

takeWhile_ :: (Token -> Bool) -> Stream -> (Tokens, Stream)
takeWhile_ = B.span
{-# INLINE takeWhile_ #-}

----------------------------------------------------------------------------
--

defaultPositionAt :: SourcePos -> a -> SourcePos
defaultPositionAt pos _ = pos
{-# INLINE defaultPositionAt #-}

defaultAdvance1
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> Word8             -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 width (SourcePos n l c) t = npos
  where
    w  = unPos width
    c' = unPos c
    npos =
      case fromEnum t of
        10 -> SourcePos n (l <> pos1) pos1
        9  -> SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
        _  -> SourcePos n l (c <> pos1)
{-# INLINE defaultAdvance1 #-}
