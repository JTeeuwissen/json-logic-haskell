module JsonLogic.Pure.Evaluator where

import Control.Monad.Identity
import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.JL
import JsonLogic.Pure.Type

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> Either String Json
eval ops rule d = runIdentity $ E.eval ops rule d

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
evalRule :: Rule -> JL Result
evalRule = E.evalRule

evalFunc :: String -> Json -> JL Result
evalFunc = E.evalFunc

subEval :: M.Map String Function -> Rule -> Data -> Result
subEval = E.subEval
