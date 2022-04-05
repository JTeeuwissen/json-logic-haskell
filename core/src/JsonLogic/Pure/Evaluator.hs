-- |
-- Module      : JsonLogic.Pure.Evaluator
-- Description : JsonLogic Pure evaluator
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Pure.Evaluator (apply, applyEmpty) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.Mapping
import JsonLogic.Pure.Operation
import JsonLogic.Pure.Type

-- | Apply takes a list of operations, a rule and data.
-- And together with the default operations evaluates it.
--
-- >>> apply [] (read "{\"cat\":[\"Hello, \", \"World!\"]}":: Json) JsonNull
-- Right "Hello, World!"
apply :: [Operation] -> Rule -> Data -> Result Json
apply ops = applyEmpty (ops ++ M.toList defaultOperations)

-- | applyEmpty takes a list of operations, a rule and data.
-- And without the default operations evaluates it.
--
-- >>> applyEmpty [] (read "{\"cat\":[\"Hello, \", \"World!\"]}":: Json) JsonNull
-- Left (UnrecognizedOperation {operationName = "cat"})
applyEmpty :: [Operation] -> Rule -> Data -> Result Json
applyEmpty ops rule dat = toResult $ E.apply (M.map fromFunction $ M.fromList ops) rule dat
