module Lightning.Text.Error.Custom
  ( CustomError
  , showCustomError
  )
where

import Data.Void

type CustomError = Void

showCustomError :: CustomError -> String
showCustomError = absurd
