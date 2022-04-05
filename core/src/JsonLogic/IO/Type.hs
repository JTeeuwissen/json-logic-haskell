-- |
-- Module      : JsonLogic.IO.Type
-- Description : JsonLogic IO types
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.IO.Type (Result, SubEvaluator, Function, Operation, Operations, throw, T.Exception (..)) where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.Json
import qualified JsonLogic.Type as T

-- | A result is an exception or a value wrapped in IO.
type Result r = IO (Either T.Exception r)

-- | A Subevaluator takes a rule and data and returns a result of JSON.
type SubEvaluator = Rule -> Data -> Result Json

-- | A function takes a subevaluator, rule and data and returns a result.
type Function r = SubEvaluator -> Rule -> Data -> Result r

-- | An operation is a JSON function with a name.
type Operation = (String, Function Json)

-- | Operations is a Map from the operation name to the operation function.
type Operations = M.Map String (Function Json)

-- | Throw an evaluation exception.
throw :: String -> Result a
throw = runExceptT . throwError . T.EvalException
