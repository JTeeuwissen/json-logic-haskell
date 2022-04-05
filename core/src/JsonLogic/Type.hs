-- |
-- Module      : JsonLogic.Type
-- Description : Internal JsonLogic types
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Type where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.Json

-- | An evaluation exception thrown by the evaluator or operations.
-- Is used in the result type.
data Exception
  = -- | Exception thrown when an unknown operation is applied.
    UnrecognizedOperation {operationName :: String}
  | -- | Exception thrown when a rule does not contain exactly one operation.
    InvalidRule {operationNames :: [String]}
  | -- | Exception thrown for any other error.
    EvalException {message :: String}
  deriving (Show, Eq)

-- | The result of a function can be an error or another Json value.
type Result m r = ExceptT Exception m r

-- | Subevaluator, with rule, its context and resulting Json.
type SubEvaluator m = Rule -> Data -> Result m Json

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function m r = SubEvaluator m -> Rule -> Data -> Result m r

-- | Operation is a function with a name.
type Operation m = (String, Function m Json)

-- | Operations is a Map from the operation name to the operation function.
type Operations m = M.Map String (Function m Json)

-- | The environment contains the functions and variables our environment has currently
data JsonLogicEnv m = JLEnv
  { operations :: Operations m, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- | Show the current environment.
instance Show (JsonLogicEnv m) where
  show (JLEnv os vs) = "Operations: " ++ show (M.keys os) ++ "\nVariables: " ++ show vs

-- | Throw an evaluation exception.
throw :: Monad m => String -> Result m a
throw = throwError . EvalException
