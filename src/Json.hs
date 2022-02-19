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

type Function = Json -> FunctionResult

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { functions :: M.Map String Function, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show _ = ""

data EvalError = EvalError
  { functionName :: String,
    paramaters :: Json,
    errorMessage :: String
  }
  deriving (Show, Eq)

type FunctionError = String

type EvalResult = Either EvalError Json

type FunctionResult = Either FunctionError Json