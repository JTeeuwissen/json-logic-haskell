module JsonLogic.Json where

import qualified Data.Map as M

-- A rule can be any kind of JSON value, but object will be evaluated.
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
type SubEvaluator = Rule -> Data -> Result

type Function = SubEvaluator -> Rule -> Data -> Result

type Operations = M.Map String Function

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show (JLEnv _ vs) = "JLEnv " ++ show vs

type Result = Either String Json
