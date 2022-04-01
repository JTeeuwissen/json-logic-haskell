{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module JsonLogic.IO.Type where

import qualified JsonLogic.Type as T

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator = T.SubEvaluator IO

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function = T.Function IO

-- | Operation is a function with a name.
type Operation = T.Operation IO

-- | Operations is a Map from the operation name to the operation function.
type Operations = T.Operations IO

-- | The environment contains the functions and variables our environment has currently
type JsonLogicEnv = T.JsonLogicEnv IO

-- | The result of a function can be an error or another json value.
type Result = T.Result IO
