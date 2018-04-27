{-# LANGUAGE DeriveDataTypeable #-}

module Lightning.State
  ( State (..) )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Lightning.Pos
import Text.Lightning.State.Custom

-- | This is the Lightning state.

data State = State
  { stateInput :: Stream
    -- ^ The rest of input to process
  , statePos :: SourcePos
    -- ^ Current position (column + line number) with support for include
    -- files
  , stateTokensProcessed :: {-# UNPACK #-} !Int
    -- ^ Number of processed tokens so far
  , stateTabWidth :: Pos
    -- ^ Tab width to use
  , stateCustom :: CustomState
    -- ^
  } deriving (Show, Eq, Data, Typeable)

instance NFData State
