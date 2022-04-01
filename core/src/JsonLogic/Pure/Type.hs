{-# LANGUAGE FlexibleInstances #-}

module JsonLogic.Pure.Type where

import Control.Monad.Identity
import qualified JsonLogic.Type as T

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator = T.SubEvaluator Identity

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function = T.Function Identity

-- | Operation is a function with a name.
type Operation = T.Operation Identity

-- | Operations is a Map from the operation name to the operation function.
type Operations = T.Operations Identity

-- | The environment contains the functions and variables our environment has currently
type JsonLogicEnv = T.JsonLogicEnv Identity

-- | The result of a function can be an error or another json value.
type Result = T.Result Identity
