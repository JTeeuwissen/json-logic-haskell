module JsonLogic.IO.Evaluator where

import Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.JL
import JsonLogic.IO.Type
import JsonLogic.Json

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> IO (Either String Json)
eval = E.eval

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
evalRule :: Rule -> JL Result
evalRule = E.evalRule

evalFunc :: String -> Json -> JL Result
evalFunc = E.evalFunc

subEval :: M.Map String Function -> Rule -> Data -> Result
subEval = E.subEval
