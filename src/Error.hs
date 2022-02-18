module Error where

import Json (Json)

data EvaluateError = Error
  { functionName :: String,
    paramaters :: [Json],
    errorMessage :: String
  }
  deriving (Show, Eq)

type FunctionError = String