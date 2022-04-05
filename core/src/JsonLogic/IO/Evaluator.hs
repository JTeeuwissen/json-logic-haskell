-- |
-- Module      : JsonLogic.IO.Evaluator
-- Description : JsonLogic IO evaluator
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.IO.Evaluator (apply, applyEmpty) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.Mapping
import JsonLogic.IO.Operation (defaultOperations)
import JsonLogic.IO.Type
import JsonLogic.Json

-- | Apply takes a list of operations, a rule and data.
-- And together with the default operations evaluates it.
-- >>> apply [] (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- "Hello, World!"
apply :: [Operation] -> Rule -> Data -> Result Json
apply ops = applyEmpty (ops ++ M.toList defaultOperations)

-- | applyEmpty takes a list of operations, a rule and data.
-- And without the default operations evaluates it.
-- >>> applyEmpty [] (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Left (UnrecognizedOperation {operationName = "log"})
applyEmpty :: [Operation] -> Rule -> Data -> Result Json
applyEmpty ops rule dat = toResult $ E.apply (M.map fromFunction $ M.fromList ops) rule dat
