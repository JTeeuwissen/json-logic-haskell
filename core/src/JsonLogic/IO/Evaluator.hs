module JsonLogic.IO.Evaluator (Result, SubEvaluator, Function, Operation, eval) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.Mapping
import JsonLogic.IO.Operation (defaultOperations)
import JsonLogic.IO.Type
import JsonLogic.Json

eval :: [Operation] -> Rule -> Data -> Result Json
eval ops rule dat = toResult $ E.eval (M.map fromFunction $ M.union (M.fromList ops) defaultOperations) rule dat
