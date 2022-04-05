module JsonLogic.Pure.Evaluator (apply, applyEmpty) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.Mapping
import JsonLogic.Pure.Operation
import JsonLogic.Pure.Type

-- >>> apply [] (read "{\"trace\":\"Hello, World!\"}":: Json) JsonNull
-- Right "Hello, World!"
apply :: [Operation] -> Rule -> Data -> Result Json
apply ops = applyEmpty (ops ++ M.toList defaultOperations)

-- >>> applyEmpty [] (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Left (UnrecognizedOperation {operationName = "log"})
applyEmpty :: [Operation] -> Rule -> Data -> Result Json
applyEmpty ops rule dat = toResult $ E.apply (M.map fromFunction $ M.fromList ops) rule dat
