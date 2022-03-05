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

-- How officially json in showed in jsonlogic
instance Show Json where
  show JsonNull = ""
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber d) = show d
  show (JsonString s) = s
  show (JsonArray js) = show js
  show (JsonObject _) = "[object Object]"

-- Pretty json print for console. Where double quotes are escaped.
prettyJson :: Json -> String
prettyJson JsonNull = "null"
prettyJson (JsonBool True) = "true"
prettyJson (JsonBool False) = "false"
prettyJson (JsonNumber d) = show d
prettyJson (JsonString s) = show s
prettyJson (JsonArray js) = show js
prettyJson (JsonObject o) = "{" ++ intercalate "," (map (\(k, v) -> k ++ ":" ++ prettyJson v) $ M.toList o) ++ "}"

isTruthy :: Json -> Bool
isTruthy JsonNull = False
isTruthy (JsonBool b) = b
isTruthy (JsonNumber 0.0) = False
isTruthy (JsonNumber _) = True
isTruthy (JsonString "") = False
isTruthy (JsonString _) = True
isTruthy (JsonArray []) = False
isTruthy (JsonArray _) = True
isTruthy (JsonObject _) = True

isFalsy :: Json -> Bool
isFalsy = not . isTruthy

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
