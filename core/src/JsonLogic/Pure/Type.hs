{-# LANGUAGE FlexibleInstances #-}

module JsonLogic.Pure.Type (Result, SubEvaluator, Function, Operation, Operations, throw, T.Exception (..)) where

import qualified Data.Map as M
import JsonLogic.Json
import qualified JsonLogic.Type as T

-- | The result of a function can be an error or another json value.
type Result r = Either T.Exception r

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator = Rule -> Data -> Result Json

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function r = SubEvaluator -> Rule -> Data -> Result r

-- | Operation is a function with a name.
type Operation = (String, Function Json)

-- | Operations is a Map from the operation name to the operation function.
type Operations = M.Map String (Function Json)

-- | Throw an evaluation exception.
throw :: String -> Result a
throw = Left . T.EvalException
