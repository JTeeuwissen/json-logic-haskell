module JsonLogic.Type where

import qualified Data.Map as M
import JsonLogic.Json

-- | A rule can be any kind of JSON value, but objects and arrays will be evaluated.
type Rule = Json

-- | Data can be any kind of JSON value.
type Data = Json

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator = Rule -> Data -> Result

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function = SubEvaluator -> Rule -> Data -> Result

-- | Operation is a function with a name.
type Operation = (String, Function)

-- | Operations is a Map from the operation name to the operation function.
type Operations = M.Map String Function

-- | The environment contains the functions and variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- | Show the current environment.
instance Show JsonLogicEnv where
  show (JLEnv os vs) = "Operations: " ++ show (M.keys os) ++ "\nVariables: " ++ show vs

-- | The result of a function can be an error or another json value.
type Result = Either String Json
