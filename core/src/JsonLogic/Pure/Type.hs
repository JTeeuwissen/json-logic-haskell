{-# LANGUAGE FlexibleInstances #-}

module JsonLogic.Pure.Type (SubEvaluator, Function, Operation, Operations, JsonLogicEnv (..), Result) where

import qualified Data.Map as M
import JsonLogic.Json

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator = Rule -> Data -> Result Json

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function r = SubEvaluator -> Rule -> Data -> Result r

-- | Operation is a function with a name.
type Operation = (String, Function Json)

-- | Operations is a Map from the operation name to the operation function.
type Operations = M.Map String (Function Json)

-- | The environment contains the functions and variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- | The result of a function can be an error or another json value.
type Result r = Either String r
