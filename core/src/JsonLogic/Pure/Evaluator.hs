module JsonLogic.Pure.Evaluator (apply, applyEmpty) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.Mapping
import JsonLogic.Pure.Operation
import JsonLogic.Pure.Type (Operations, Result)

-- >>> apply M.empty(read "{\"trace\":\"Hello, World!\"}":: Json) JsonNull
-- Right "Hello, World!"
apply :: Operations -> Rule -> Data -> Result Json
apply ops = applyEmpty (M.union ops defaultOperations)

-- >>> applyEmpty M.empty (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Left (UnrecognizedOperation {operationName = "log"})
applyEmpty :: Operations -> Rule -> Data -> Result Json
applyEmpty ops rule dat = toResult $ E.apply (M.map fromFunction ops) rule dat
