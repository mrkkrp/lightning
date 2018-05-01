module Lightning
  ( -- * Re-exports
    module Text.Megaparsec.Pos
  , module Text.Megaparsec.Error
  , module Control.Monad.Combinators
    -- * Data types
  , State (..)
  , Parser
    -- * Running parser
  -- , parse
  -- , parseMaybe
  -- , parseTest
  -- , runParser
  -- , runParser'
  --   -- * Primitive combinators
  -- , MonadParsec (..)
  --   -- * Derivatives of primitive combinators
  -- , single
  -- , satisfy
  -- , anySingle
  -- , anySingleBut
  -- , oneOf
  -- , noneOf
  -- , chunk
  -- , (<?>)
  -- , unexpected
  -- , customFailure
  -- , match
  -- , region
  -- , takeRest
  -- , atEnd
  --   -- * Parser state combinators
  -- , getInput
  -- , setInput
  -- , getPosition
  -- , getNextTokenPosition
  -- , setPosition
  -- , getTokensProcessed
  -- , setTokensProcessed
  -- , getTabWidth
  -- , setTabWidth
  -- , setParserState
  )
where

import Lightning.Internal
import Lightning.State
import Lightning.Stream

----------------------------------------------------------------------------
-- Data types

----------------------------------------------------------------------------
-- Running a parser

-- parse
--   :: Parsec e s a -- ^ Parser to run
--   -> String       -- ^ Name of source file
--   -> s            -- ^ Input for parser
--   -> Either (ParseError (Token s) e) a
-- parse = runParser

-- parseMaybe :: (Ord e, Stream s) => Parsec e s a -> s -> Maybe a
-- parseMaybe p s =
--   case parse (p <* eof) "" s of
--     Left  _ -> Nothing
--     Right x -> Just x

-- parseTest :: ( ShowErrorComponent e
--              , Ord (Token s)
--              , ShowToken (Token s)
--              , Show a )
--   => Parsec e s a -- ^ Parser to run
--   -> s            -- ^ Input for parser
--   -> IO ()
-- parseTest p input =
--   case parse p "" input of
--     Left  e -> putStr (parseErrorPretty e)
--     Right x -> print x

-- parseTest' :: ( ShowErrorComponent e
--               , ShowToken (Token s)
--               , LineToken (Token s)
--               , Show a
--               , Stream s )
--   => Parsec e s a -- ^ Parser to run
--   -> s            -- ^ Input for parser
--   -> IO ()
-- parseTest' p input =
--   case parse p "" input of
--     Left  e -> putStr (parseErrorPretty' input e)
--     Right x -> print x

-- runParser
--   :: Parsec e s a -- ^ Parser to run
--   -> String     -- ^ Name of source file
--   -> s          -- ^ Input for parser
--   -> Either (ParseError (Token s) e) a
-- runParser p name s = snd $ runParser' p (initialState name s)

-- runParser'
--   :: Parsec e s a -- ^ Parser to run
--   -> State s    -- ^ Initial state
--   -> (State s, Either (ParseError (Token s) e) a)
-- runParser' p = runIdentity . runParserT' p

-- runParserT :: Monad m
--   => ParsecT e s m a -- ^ Parser to run
--   -> String        -- ^ Name of source file
--   -> s             -- ^ Input for parser
--   -> m (Either (ParseError (Token s) e) a)
-- runParserT p name s = snd `liftM` runParserT' p (initialState name s)

-- runParserT' :: Monad m
--   => ParsecT e s m a -- ^ Parser to run
--   -> State s       -- ^ Initial state
--   -> m (State s, Either (ParseError (Token s) e) a)
-- runParserT' p s = do
--   (Reply s' _ result) <- runParsecT p s
--   case result of
--     OK    x -> return (s', Right x)
--     Error e -> return (s', Left  e)

-- -- | Given name of source file and input construct initial state for parser.

-- initialState :: String -> s -> State s
-- initialState name s = State
--   { stateInput           = s
--   , statePos             = initialPos name
--   , stateTokensProcessed = 0
--   , stateTabWidth        = defaultTabWidth }
