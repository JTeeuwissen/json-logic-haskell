{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module JsonLogic.IO.Type where

import qualified Data.Map as M
import JsonLogic.Json
import JsonLogic.Type

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluatorIO = Rule -> Data -> ResultIO

subEvaluatorIOFromPure :: SubEvaluator -> SubEvaluatorIO
subEvaluatorIOFromPure subEvaluator rule d = pure $ subEvaluator rule d

-- | A function takes a subevaluator, a rule and data and returns a result.
type FunctionIO = SubEvaluatorIO -> Rule -> Data -> ResultIO

functionIOFromPure :: Function -> FunctionIO
functionIOFromPure function subIO rule d = do
  pure $ function sub' rule d
  where
    sub' :: SubEvaluator
    sub' = undefined --TODO

-- | Operation is a function with a name.
type OperationIO = (String, FunctionIO)

-- | Operations is a Map from the operation name to the operation function.
type OperationsIO = M.Map String FunctionIO

-- | The environment contains the functions and variables our environment has currently
data JsonLogicEnvIO = JLEnvIO
  { operations :: OperationsIO, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- | Show the current environment.
instance Show JsonLogicEnvIO where
  show (JLEnvIO os vs) = "Operations: " ++ show (M.keys os) ++ "\nVariables: " ++ show vs

-- | The result of a function can be an error or another json value.
type ResultIO = IO (Either String Json)
