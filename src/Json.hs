module Json where

import qualified Data.Map as M

-- A rule can only be a JSON object.
type Rule = Object

-- Data can be any kind of JSON value.
type Data = Json

-- Json is a collection of possivle JSON values.
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject Object
  deriving (Eq, Show)

-- A Json object, dictionary with string keys and Json values.
type Object = (M.Map String Json)

-- Subevaluator, with rule, its context and retulting json.
type SubEvaluator = Rule -> Data -> EvalResult

type Function = SubEvaluator -> Json -> FunctionResult

type Operations = M.Map String Function

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show (JLEnv _ vs) = "JLEnv " ++ show vs

data EvalError = EvalError
  { functionName :: String,
    paramaters :: Json,
    functionError :: FunctionError
  }
  deriving (Show, Eq)

-- Message and possible inner exception
data FunctionError = FunctionError
  { functionErrorMessage :: String,
    innerError ::
      Maybe EvalError
  }
  deriving (Show, Eq)

type EvalResult = Either EvalError Json

type FunctionResult = Either FunctionError Json
