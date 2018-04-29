module Lightning.Error.Class
  ( ShowToken (..) )
where

import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.List.NonEmpty as NE

class ShowToken a where

  showTokens :: NonEmpty a -> String
  tokenAsChar :: a -> Char
  tokenIsNewline :: a -> Bool

instance ShowToken Char where
  showTokens = stringPretty
  tokenAsChar = id
  tokenIsNewline x = x == '\n'

instance ShowToken Word8 where
  showTokens = stringPretty . fmap (chr . fromIntegral)
  tokenAsChar = chr . fromIntegral
  tokenIsNewline x = x == 10

----------------------------------------------------------------------------
-- Helpers

-- | @stringPretty s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> String
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing     -> [ch]
        Just pretty -> "<" <> pretty <> ">"

-- | @charPretty ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages.

charPretty :: Char -> String
charPretty ' ' = "space"
charPretty ch = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

charPretty' :: Char -> Maybe String
charPretty' '\NUL' = pure "null"
charPretty' '\SOH' = pure "start of heading"
charPretty' '\STX' = pure "start of text"
charPretty' '\ETX' = pure "end of text"
charPretty' '\EOT' = pure "end of transmission"
charPretty' '\ENQ' = pure "enquiry"
charPretty' '\ACK' = pure "acknowledge"
charPretty' '\BEL' = pure "bell"
charPretty' '\BS'  = pure "backspace"
charPretty' '\t'   = pure "tab"
charPretty' '\n'   = pure "newline"
charPretty' '\v'   = pure "vertical tab"
charPretty' '\f'   = pure "form feed"
charPretty' '\r'   = pure "carriage return"
charPretty' '\SO'  = pure "shift out"
charPretty' '\SI'  = pure "shift in"
charPretty' '\DLE' = pure "data link escape"
charPretty' '\DC1' = pure "device control one"
charPretty' '\DC2' = pure "device control two"
charPretty' '\DC3' = pure "device control three"
charPretty' '\DC4' = pure "device control four"
charPretty' '\NAK' = pure "negative acknowledge"
charPretty' '\SYN' = pure "synchronous idle"
charPretty' '\ETB' = pure "end of transmission block"
charPretty' '\CAN' = pure "cancel"
charPretty' '\EM'  = pure "end of medium"
charPretty' '\SUB' = pure "substitute"
charPretty' '\ESC' = pure "escape"
charPretty' '\FS'  = pure "file separator"
charPretty' '\GS'  = pure "group separator"
charPretty' '\RS'  = pure "record separator"
charPretty' '\US'  = pure "unit separator"
charPretty' '\DEL' = pure "delete"
charPretty' '\160' = pure "non-breaking space"
charPretty' _      = Nothing
