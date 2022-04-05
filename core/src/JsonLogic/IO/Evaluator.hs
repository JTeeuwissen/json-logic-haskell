module JsonLogic.IO.Evaluator (apply, applyEmpty) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.Mapping
import JsonLogic.IO.Operation (defaultOperations)
import JsonLogic.IO.Type
import JsonLogic.Json

-- >>> apply [] (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Right "Hello, World!"
apply :: [Operation] -> Rule -> Data -> Result Json
apply ops = applyEmpty (ops ++ M.toList defaultOperations)

-- >>> applyEmpty [] (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Left (UnrecognizedOperation {operationName = "log"})
applyEmpty :: [Operation] -> Rule -> Data -> Result Json
applyEmpty ops rule dat = toResult $ E.apply (M.map fromFunction $ M.fromList ops) rule dat
