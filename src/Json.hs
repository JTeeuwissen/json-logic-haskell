module Json where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.Map as M

-- import Json (Function, Json, JsonLogicEnv (operations, variables), Operations)

-- A rule can only be a JSON object.
type Rule = Json

-- Data can be any kind of JSON value.
type Data = Json

-- Json is a collection of possivle JSON values.
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject (M.Map String Json)
  deriving (Eq, Show)

-- Subevaluator, with rule, its context and retulting json.
type SubEvaluator = Rule -> Data -> EvalResult

type Function = Json -> JL Json

type Operations = M.Map String Function

-- For convencience
type CreateError = String -> JLError

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show (JLEnv _ vs) = "JLEnv " ++ show vs

data JLError = JLError
  { functionName :: String,
    errorMessage :: String
  }
  deriving (Show, Eq)

type EvalResult = Either JLError Json

type FunctionResult = Either JLError Json

-- This belonged somewher else

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
-- type JL a = Reader JsonLogicEnv a
type JL a = ReaderT JsonLogicEnv (Except String) a

getFunction :: String -> JL (Maybe Function)
getFunction name = asks (M.lookup name . operations)

getOperations :: JL Operations
getOperations = asks operations

getVariables :: JL Json
getVariables = asks variables
