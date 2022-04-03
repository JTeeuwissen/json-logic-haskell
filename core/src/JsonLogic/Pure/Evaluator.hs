module JsonLogic.Pure.Evaluator (eval) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.Mapping
import JsonLogic.Pure.Operation
import JsonLogic.Pure.Type

-- >>> eval [] (read "{\"trace\":\"Hello, World!\"}":: Json) JsonNull
-- Right "Hello, World!"
eval :: [Operation] -> Rule -> Data -> Result Json
eval ops rule d = toResult $ E.eval (M.map fromFunction (M.union (M.fromList ops) defaultOperations)) rule d
