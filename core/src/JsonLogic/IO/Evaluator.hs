module JsonLogic.IO.Evaluator (eval) where

import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.Operation (defaultOperations)
import JsonLogic.IO.Type
import JsonLogic.Json

eval :: [Operation] -> Rule -> Data -> Result Json
eval ops = E.eval (M.union (M.fromList ops) defaultOperations)
