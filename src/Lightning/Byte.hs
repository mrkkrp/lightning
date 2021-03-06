{-# LANGUAGE TypeFamilies #-}

module Lightning.Byte
  ( -- * Simple parsers
    newline
  , crlf
  , eol
  , tab
  , space
  , space1
    -- * Categories of characters
  , controlChar
  , spaceChar
  -- , upperChar
  -- , lowerChar
  -- , letterChar
  -- , alphaNumChar
  -- , printChar
  , digitChar
  -- , octDigitChar
  -- , hexDigitChar
  -- , asciiChar
    -- * Single byte
  , char
  , char'
    -- * Sequence of bytes
  , string
  -- , C.string'
  )
where

import Control.Applicative
import Data.Char
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Lightning
import Lightning.Stream

----------------------------------------------------------------------------
-- Simple parsers

-- | Parse a newline byte.

newline :: Token ~ Word8 => Parser Token
newline = char (10 :: Word8)
{-# INLINE newline #-}

-- | Parse a carriage return character followed by a newline character.
-- Return the sequence of characters parsed.

crlf :: Token ~ Word8 => Parser Tokens
crlf = string (tokensToChunk ([13,10] :: [Word8]))
{-# INLINE crlf #-}

-- | Parse a CRLF (see 'crlf') or LF (see 'newline') end of line. Return the
-- sequence of characters parsed.

eol :: Token ~ Word8 => Parser Tokens
eol = (tokenToChunk <$> newline) <|> crlf <?> "end of line"
{-# INLINE eol #-}

-- | Parse a tab character.

tab :: Token ~ Word8 => Parser Token
tab = char (9 :: Word8)
{-# INLINE tab #-}

-- | Skip /zero/ or more white space characters.
--
-- See also: 'skipMany' and 'spaceChar'.

space :: Token ~ Word8 => Parser ()
space = void $ takeWhileP (Just "white space") isSpace'
{-# INLINE space #-}

-- | Skip /one/ or more white space characters.
--
-- See also: 'skipSome' and 'spaceChar'.

space1 :: Token ~ Word8 => Parser ()
space1 = void $ takeWhile1P (Just "white space") isSpace'
{-# INLINE space1 #-}

----------------------------------------------------------------------------
-- Categories of characters

-- | Parse a control character.

controlChar :: Token ~ Word8 => Parser Token
controlChar = satisfy (isControl . toChar) <?> "control character"
{-# INLINE controlChar #-}

-- | Parse a space character, and the control characters: tab, newline,
-- carriage return, form feed, and vertical tab.

spaceChar :: Token ~ Word8 => Parser Token
spaceChar = satisfy isSpace' <?> "white space"
{-# INLINE spaceChar #-}

-- -- | Parse an upper-case character.

-- upperChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- upperChar = satisfy (isUpper . toChar) <?> "uppercase letter"
-- {-# INLINE upperChar #-}

-- -- | Parse a lower-case alphabetic character.

-- lowerChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- lowerChar = satisfy (isLower . toChar) <?> "lowercase letter"
-- {-# INLINE lowerChar #-}

-- -- | Parse an alphabetic character: lower-case or upper-case.

-- letterChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- letterChar = satisfy (isLetter . toChar) <?> "letter"
-- {-# INLINE letterChar #-}

-- -- | Parse an alphabetic or digit characters.

-- alphaNumChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- alphaNumChar = satisfy (isAlphaNum . toChar) <?> "alphanumeric character"
-- {-# INLINE alphaNumChar #-}

-- -- | Parse a printable character: letter, number, mark, punctuation, symbol
-- -- or space.

-- printChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- printChar = satisfy (isPrint . toChar) <?> "printable character"
-- {-# INLINE printChar #-}

-- | Parse an ASCII digit, i.e between “0” and “9”.

digitChar :: Token ~ Word8 => Parser Token
digitChar = satisfy isDigit' <?> "digit"
  where
    isDigit' :: Word8 -> Bool
    isDigit' x = x >= 48 && x <= 57
{-# INLINE digitChar #-}

-- -- | Parse an octal digit, i.e. between “0” and “7”.

-- octDigitChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- octDigitChar = satisfy isOctDigit' <?> "octal digit"
--   where
--     isOctDigit' x = x >= 48 && x <= 55
-- {-# INLINE octDigitChar #-}

-- -- | Parse a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”, or
-- -- “A” and “F”.

-- hexDigitChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- hexDigitChar = satisfy (isHexDigit . toChar) <?> "hexadecimal digit"
-- {-# INLINE hexDigitChar #-}

-- -- | Parse a character from the first 128 characters of the Unicode
-- -- character set, corresponding to the ASCII character set.

-- asciiChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
-- asciiChar = satisfy (< 128) <?> "ASCII character"
-- {-# INLINE asciiChar #-}

----------------------------------------------------------------------------
-- Single byte

-- | A type-constrained version of 'single'.
--
-- > newline = char 10

char :: Token ~ Word8 => Token -> Parser Token
char = single
{-# INLINE char #-}

-- | The same as 'char' but case-insensitive. This parser returns the
-- actually parsed character preserving its case.
--
-- >>> parseTest (char' 101) "E"
-- 69 -- 'E'
-- >>> parseTest (char' 101) "G"
-- 1:1:
-- unexpected 'G'
-- expecting 'E' or 'e'

char' :: Token ~ Word8 => Token -> Parser Token
char' c = choice
  [ char c
  , char (fromMaybe c (swapCase c)) ]
  where
    swapCase x
      | isUpper g = fromChar (toLower g)
      | isLower g = fromChar (toUpper g)
      | otherwise = Nothing
      where
        g = toChar x
{-# INLINE char' #-}

----------------------------------------------------------------------------
-- Sequence of bytes

-- | A type-constrained version of 'chunk'.

string :: Token ~ Word8 => Tokens -> Parser Tokens
string = chunk
{-# INLINE string #-}

----------------------------------------------------------------------------
-- Helpers

-- | 'Word8'-specialized version of 'isSpace'.

isSpace' :: Word8 -> Bool
isSpace' x
  | x >= 9 && x <= 13 = True
  | x == 32           = True
  | x == 160          = True
  | otherwise         = False
{-# INLINE isSpace' #-}

-- | Convert a byte to char.

toChar :: Word8 -> Char
toChar = chr . fromIntegral
{-# INLINE toChar #-}

-- | Convert a char to byte.

fromChar :: Char -> Maybe Word8
fromChar x = let p = ord x in
  if p > 0xff
    then Nothing
    else Just (fromIntegral p)
{-# INLINE fromChar #-}
