{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Lightning.Error
  ( -- * Parse error type
    ErrorItem (..)
  , ErrorFancy (..)
  , ParseError (..)
  , errorPos
  , ShowToken (..)
    -- * Pretty-printing
  , parseErrorPretty
  , parseErrorPretty'
  , parseErrorPretty_
  , parseErrorTextPretty )
where

import Control.DeepSeq
import Control.Exception
import Data.Char (chr)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Maybe (isNothing)
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics
import Lightning.Error.Custom
import Lightning.Stream
import Prelude hiding (concat)
import Text.Pos
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

----------------------------------------------------------------------------
-- Parse error type

-- | Data type that is used to represent “unexpected\/expected” items in
-- 'ParseError'.

data ErrorItem
  = Tokens (NonEmpty Token)  -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Eq, Ord, Typeable, Generic)

instance NFData ErrorItem

-- | Additional error data.

data ErrorFancy
  = ErrorFail String
    -- ^ 'fail' has been used in parser monad
  | ErrorIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  | ErrorCustom CustomError
    -- ^ Custom error data
  deriving (Show, Eq, Ord, Typeable, Generic)

instance NFData ErrorFancy

-- | @'ParseError' t e@ represents a parse error parametrized over the token
-- type @t@ and the custom data @e@.
--
-- Note that the stack of source positions contains current position as its
-- head, and the rest of positions allows to track full sequence of include
-- files with topmost source file at the end of the list.
--
-- 'Semigroup' and 'Monoid' instances of the data type allow to merge parse
-- errors from different branches of parsing. When merging two
-- 'ParseError's, the longest match is preferred; if positions are the same,
-- custom data sets and collections of message items are combined. Note that
-- fancy errors take precedence over trivial errors in merging.

data ParseError
  = TrivialError SourcePos (Maybe ErrorItem) (Set ErrorItem)
    -- ^ Trivial errors, generated by Megaparsec's machinery. The data
    -- constructor includes the source position of error, unexpected token
    -- (if any), and expected tokens.
  | FancyError SourcePos (Set ErrorFancy)
    -- ^ Fancy, custom errors.
  deriving (Show, Eq, Typeable, Generic)

instance NFData ParseError

instance Semigroup ParseError where
  (<>) = mergeError
  {-# INLINE (<>) #-}

instance Monoid ParseError where
  mempty  = TrivialError (initialPos "") Nothing E.empty
  mappend = (<>)
  {-# INLINE mappend #-}

instance ShowToken Token => Exception ParseError where
  displayException = parseErrorPretty

-- | Get position of given 'ParseError'.

errorPos :: ParseError -> SourcePos
errorPos (TrivialError p _ _) = p
errorPos (FancyError   p _)   = p

-- | Merge two error data structures into one joining their collections of
-- message items and preferring the longest match. In other words, earlier
-- error message is discarded. This may seem counter-intuitive, but
-- 'mergeError' is only used to merge error messages of alternative branches
-- of parsing and in this case longest match should be preferred.

mergeError :: ParseError -> ParseError -> ParseError
mergeError e1 e2 =
  case errorPos e1 `compare` errorPos e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          TrivialError s1 (n u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
  where
    -- NOTE The logic behind this merging is that since we only combine
    -- parse errors that happen at exactly the same position, all the
    -- unexpected items will be prefixes of input stream at that position or
    -- labels referring to the same thing. Our aim here is to choose the
    -- longest prefix (merging with labels and end of input is somewhat
    -- arbitrary, but is necessary because otherwise we can't make
    -- ParseError lawful Monoid and have nice parse errors at the same
    -- time).
    n Nothing  Nothing = Nothing
    n (Just x) Nothing = Just x
    n Nothing (Just y) = Just y
    n (Just x) (Just y) = Just (max x y)
{-# INLINE mergeError #-}

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
-- Pretty-printing

parseErrorPretty :: ShowToken Token
  => ParseError        -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorPretty e =
  sourcePosPretty (errorPos e) <> ":\n" <> parseErrorTextPretty e

parseErrorPretty' :: ShowToken Token
  => Stream            -- ^ Original input stream
  -> ParseError        -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorPretty' = parseErrorPretty_ defaultTabWidth

parseErrorPretty_ :: ShowToken Token
  => Pos               -- ^ Tab width
  -> Stream            -- ^ Original input stream
  -> ParseError        -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorPretty_ w s e =
  sourcePosPretty (errorPos e) <> ":\n" <>
    padding <> "|\n" <>
    lineNumber <> " | " <> rline <> "\n" <>
    padding <> "| " <> rpadding <> "^\n" <>
    parseErrorTextPretty e
  where
    epos       = errorPos e
    lineNumber = (show . unPos . sourceLine) epos
    padding    = replicate (length lineNumber + 1) ' '
    rpadding   = replicate (unPos (sourceColumn epos) - 1) ' '
    rline      =
      case rline' of
        "" -> "<empty line>"
        xs -> expandTab w xs
    rline'     = fmap tokenAsChar . chunkToTokens $
      selectLine (sourceLine epos) s

parseErrorTextPretty :: ShowToken Token
  => ParseError        -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorTextPretty (TrivialError _ us ps) =
  if isNothing us && E.null ps
    then "unknown parse error\n"
    else messageItemsPretty "unexpected "
           (E.map showErrorItem $ maybe E.empty E.singleton us) <>
         messageItemsPretty "expecting "
           (E.map showErrorItem ps)
parseErrorTextPretty (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error\n"
    else unlines (showErrorComponent <$> E.toAscList xs)

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

----------------------------------------------------------------------------
-- Helpers

showErrorItem :: ShowToken Token => ErrorItem -> String
showErrorItem = \case
  Tokens ts -> showTokens ts
  Label label -> NE.toList label
  EndOfInput -> "end of input"

showErrorComponent :: ErrorFancy -> String
showErrorComponent = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got " <> show (unPos actual) <>
    ", should be " <> p <> show (unPos ref) <> ")"
    where
      p = case ord of
            LT -> "less than "
            EQ -> "equal to "
            GT -> "greater than "
  ErrorCustom a -> showCustomError a

-- | Transforms a list of error messages into their textual representation.

messageItemsPretty
  :: String            -- ^ Prefix to prepend
  -> Set String        -- ^ Collection of messages
  -> String            -- ^ Result of rendering
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
    let f = orList . NE.fromList . E.toAscList
    in prefix <> f ts <> "\n"

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to the rules of English punctuation.

orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

-- | Select a line from input stream given its number.

selectLine :: ShowToken Token
  => Pos               -- ^ Number of line to select
  -> Stream            -- ^ Input stream
  -> Tokens            -- ^ Selected line
selectLine l = go pos1
  where
    go !n !s =
      if n == l
        then fst (takeWhile_ notNewline s)
        else go (n <> pos1) (stripNewline $ snd (takeWhile_ notNewline s))
    notNewline = not . tokenIsNewline
    stripNewline s =
      case take1_ s of
        Nothing -> s
        Just (_, s') -> s'

-- | Replace tab characters with given number of spaces.

expandTab
  :: Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go !n xs       = ' ' : go (n - 1) xs
    w              = unPos w'
