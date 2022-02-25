module JsonLogic.Json where

import Data.List (intercalate)
import qualified Data.Map as M (Map, toList)

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
  deriving (Eq)

-- Simple instance to show Json, does not format it nicely.
instance Show Json where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber d) = show d
  show (JsonString s) = show s
  show (JsonArray js) = show js
  show (JsonObject o) = "{" ++ intercalate "," (map (\(k, v) -> show k ++ ":" ++ show v) $ M.toList o) ++ "}"

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
