-- |
-- Module      : JsonLogic.Pure.Evaluator
-- Description : JsonLogic Pure evaluator
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Pure.Evaluator (apply) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.Mapping
import JsonLogic.Pure.Operation
import JsonLogic.Pure.Type

-- >>> apply [] (read "{\"trace\":\"Hello, World!\"}":: Json) JsonNull
-- Right "Hello, World!"
apply :: [Operation] -> Rule -> Data -> Result Json
apply ops rule d = toResult $ E.apply (M.map fromFunction (M.union (M.fromList ops) defaultOperations)) rule d
