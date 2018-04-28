{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Lightning.State
  ( State (..) )
where

import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import GHC.Generics
import Lightning.Pos
import Lightning.State.Custom
import Lightning.Stream

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
  } deriving (Show, Eq, Typeable, Generic)

instance NFData State
