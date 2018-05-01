{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Lightning.State
  ( State (..) )
where

import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import GHC.Generics
import Lightning.State.Custom
import Lightning.Stream
import Text.Pos

-- | This is the Lightning state.

data State = State
  { stInput :: {-# UNPACK #-} !Stream
    -- ^ The rest of input to process
  , stPos :: SourcePos
    -- ^ Current position (column + line number) with support for include
    -- files
  , stTP :: {-# UNPACK #-} !Int
    -- ^ Number of processed tokens so far
  , stTabWidth :: Pos
    -- ^ Tab width to use
  , stCustom :: {-# UNPACK #-} !CustomState
    -- ^ Custom state component
  } deriving (Show, Eq, Typeable, Generic)

instance NFData State
