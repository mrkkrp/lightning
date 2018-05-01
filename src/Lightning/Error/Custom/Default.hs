module Lightning.Error.Custom.Default
  ( CustomError
  , showCustomError
  )
where

import Data.Void

type CustomError = Void

showCustomError :: CustomError -> String
showCustomError = absurd
