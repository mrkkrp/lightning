{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Lightning.Byte.Lexer
  ( -- * White space
  --   C.space
    lexeme
  -- , C.symbol
  -- , C.symbol'
  -- , skipLineComment
  -- , skipBlockComment
  -- , skipBlockCommentNested
    -- * Numbers
  , decimal
  , octal
  , hexadecimal
  , scientific
  , float
  , signed )
where

import Control.Applicative
import Data.Functor (void)
import Data.List (foldl')
import Data.Scientific (Scientific)
import Data.Word (Word8)
import Lightning
import Lightning.Byte
import Lightning.Stream
import qualified Data.Scientific as Sci

----------------------------------------------------------------------------
-- White space

lexeme
  :: Parser ()         -- ^ How to consume white space after lexeme
  -> Parser a          -- ^ How to parse actual lexeme
  -> Parser a
lexeme spc p = p <* spc

-- -- | Given comment prefix this function returns a parser that skips line
-- -- comments. Note that it stops just before the newline character but
-- -- doesn't consume the newline. Newline is either supposed to be consumed by
-- -- 'space' parser or picked up manually.

-- skipLineComment :: (MonadParsec e s m, Token s ~ Word8)
--   => Tokens s          -- ^ Line comment prefix
--   -> m ()
-- skipLineComment prefix =
--   string prefix *> void (takeWhileP (Just "character") (/= 10))
-- {-# INLINEABLE skipLineComment #-}

-- -- | @'skipBlockComment' start end@ skips non-nested block comment starting
-- -- with @start@ and ending with @end@.

-- skipBlockComment :: (MonadParsec e s m, Token s ~ Word8)
--   => Tokens s          -- ^ Start of block comment
--   -> Tokens s          -- ^ End of block comment
--   -> m ()
-- skipBlockComment start end = p >> void (manyTill anySingle n)
--   where
--     p = string start
--     n = string end
-- {-# INLINEABLE skipBlockComment #-}

-- -- | @'skipBlockCommentNested' start end@ skips possibly nested block
-- -- comment starting with @start@ and ending with @end@.
-- --
-- -- @since 5.0.0

-- skipBlockCommentNested :: (MonadParsec e s m, Token s ~ Word8)
--   => Tokens s          -- ^ Start of block comment
--   -> Tokens s          -- ^ End of block comment
--   -> m ()
-- skipBlockCommentNested start end = p >> void (manyTill e n)
--   where
--     e = skipBlockCommentNested start end <|> void anySingle
--     p = string start
--     n = string end
-- {-# INLINEABLE skipBlockCommentNested #-}

----------------------------------------------------------------------------
-- Numbers

-- | Parse an integer in decimal representation according to the format of
-- integer literals described in the Haskell report.
--
-- If you need to parse signed integers, see 'signed' combinator.

decimal :: (Token ~ Word8, Integral a) => Parser a
decimal = decimal_ <?> "integer"
{-# INLINEABLE decimal #-}

-- | A non-public helper to parse decimal integers.

decimal_ :: (Token ~ Word8, Integral a) => Parser a
decimal_ = mkNum <$> takeWhile1P (Just "digit") isDigit
  where
    mkNum    = foldl' step 0 . chunkToTokens
    step a w = a * 10 + fromIntegral ((w :: Word8) - 48)

-- | Parse an integer in octal representation. Representation of octal
-- number is expected to be according to the Haskell report except for the
-- fact that this parser doesn't parse “0o” or “0O” prefix. It is a
-- responsibility of the programmer to parse correct prefix before parsing
-- the number itself.
--
-- For example you can make it conform to the Haskell report like this:
--
-- > octal = char '0' >> char' 'o' >> L.octal

octal :: (Token ~ Word8, Integral a) => Parser a
octal = mkNum
  <$> takeWhile1P Nothing isOctDigit
  <?> "octal integer"
  where
    mkNum        = foldl' step 0 . chunkToTokens
    step a w     = a * 8 + fromIntegral ((w :: Word8) - 48)
    isOctDigit :: Word8 -> Bool
    isOctDigit w = w - 48 < 8
{-# INLINEABLE octal #-}

-- | Parse an integer in hexadecimal representation. Representation of
-- hexadecimal number is expected to be according to the Haskell report
-- except for the fact that this parser doesn't parse “0x” or “0X” prefix.
-- It is a responsibility of the programmer to parse correct prefix before
-- parsing the number itself.
--
-- For example you can make it conform to the Haskell report like this:
--
-- > hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

hexadecimal :: forall a. (Token ~ Word8, Integral a) => Parser a
hexadecimal = mkNum
  <$> takeWhile1P Nothing isHexDigit
  <?> "hexadecimal integer"
  where
    mkNum        = foldl' step 0 . chunkToTokens
    step :: a -> Word8 -> a
    step a w
      | w >= 48 && w <= 57 = a * 16 + fromIntegral (w - 48)
      | w >= 97            = a * 16 + fromIntegral (w - 87)
      | otherwise          = a * 16 + fromIntegral (w - 55)
    isHexDigit :: Word8 -> Bool
    isHexDigit w =
      (w >= 48 && w <= 57)  ||
      (w >= 97 && w <= 102) ||
      (w >= 65 && w <= 70)
{-# INLINEABLE hexadecimal #-}

-- | Parse a floating point value as a 'Scientific' number. 'Scientific' is
-- great for parsing of arbitrary precision numbers coming from an untrusted
-- source. See documentation in "Data.Scientific" for more information.
--
-- The parser can be used to parse integers or floating point values. Use
-- functions like 'Data.Scientific.floatingOrInteger' from "Data.Scientific"
-- to test and extract integer or real values.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.

scientific :: Token ~ Word8 => Parser Scientific
scientific = do
  c'      <- decimal_
  SP c e' <- option (SP c' 0) (try $ dotDecimal_ c')
  e       <- option e' (try $ exponent_ e')
  return (Sci.scientific c e)
{-# INLINEABLE scientific #-}

data SP = SP !Integer {-# UNPACK #-} !Int

-- | Parse a floating point number according to the syntax for floating
-- point literals described in the Haskell report.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.
--
-- __Note__: in versions 6.0.0–6.1.1 this function accepted plain integers.

float :: (Token ~ Word8, RealFloat a) => Parser a
float = do
  c' <- decimal_
  Sci.toRealFloat <$>
    ((do SP c e' <- dotDecimal_ c'
         e       <- option e' (try $ exponent_ e')
         return (Sci.scientific c e))
     <|> (Sci.scientific c' <$> exponent_ 0))
{-# INLINEABLE float #-}

dotDecimal_ :: Token ~ Word8 => Integer -> Parser SP
dotDecimal_ c' = do
  void (char (46 :: Word8))
  let mkNum = foldl' step (SP c' 0) . chunkToTokens
      step (SP a e') w = SP
        (a * 10 + fromIntegral ((w :: Word8) - 48))
        (e' - 1)
  mkNum <$> takeWhile1P (Just "digit") isDigit
{-# INLINE dotDecimal_ #-}

exponent_ :: Token ~ Word8 => Int -> Parser Int
exponent_ e' = do
  void (char' (101 :: Word8))
  (+ e') <$> signed (return ()) decimal_
{-# INLINE exponent_ #-}

-- | @'signed' space p@ parser parses an optional sign character (“+” or
-- “-”), then if there is a sign it consumes optional white space (using
-- @space@ parser), then it runs parser @p@ which should return a number.
-- Sign of the number is changed according to the previously parsed sign
-- character.
--
-- For example, to parse signed integer you can write:
--
-- > lexeme        = L.lexeme spaceConsumer
-- > integer       = lexeme L.decimal
-- > signedInteger = L.signed spaceConsumer integer

signed :: (Token ~ Word8, Num a)
  => Parser ()         -- ^ How to consume white space after the sign
  -> Parser a          -- ^ How to parse the number itself
  -> Parser a          -- ^ Parser for signed numbers
signed spc p = ($) <$> option id (lexeme spc sign) <*> p
  where
    sign = (id <$ char (43 :: Word8)) <|> (negate <$ char (45 :: Word8))
{-# INLINEABLE signed #-}

----------------------------------------------------------------------------
-- Helpers

-- | A fast predicate to check if given 'Word8' is a digit in ASCII.

isDigit :: Word8 -> Bool
isDigit w = w - 48 < 10
{-# INLINE isDigit #-}
