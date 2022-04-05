module JsonLogic.IO.Evaluator (apply) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.Mapping
import JsonLogic.IO.Operation (defaultOperations)
import JsonLogic.IO.Type
import JsonLogic.Json

-- >>> apply M.empty (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Right "Hello, World!"
apply :: Operations -> Rule -> Data -> Result Json
apply ops = applyEmpty (M.union ops defaultOperations)

-- >>> applyEmpty M.empty (read "{\"log\":\"Hello, World!\"}":: Json) JsonNull
-- Left (UnrecognizedOperation {operationName = "log"})
applyEmpty :: Operations -> Rule -> Data -> Result Json
applyEmpty ops rule dat = toResult $ E.apply (M.map fromFunction ops) rule dat
