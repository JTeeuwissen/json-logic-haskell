module JsonLogic.IO.Evaluator where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M
import JsonLogic.IO.JL
import JsonLogic.IO.Operation
import JsonLogic.IO.Type
import JsonLogic.Json
import JsonLogic.Type (Data, Rule)

-- evaluate JsonLogic without bothering about monads
evalIO :: [OperationIO] -> Rule -> Data -> ResultIO
evalIO ops rule d = runReader (evalRuleIO rule) $ createEnvIO (M.fromList ops) d

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
evalRuleIO :: Rule -> JLIO ResultIO
evalRuleIO o@(JsonObject rule) = do
  result <- sequenceA <$> traverseWithKey evalFuncIO rule
  -- An empty rule returns itself
  return $ M.foldr const o <$> result -- FIX: this only returns a single rule result. Maybe evaluate one.
evalRuleIO (JsonArray rules) = do
  result <- sequenceA <$> mapM evalRule rules
  return $ JsonArray <$> result
evalRuleIO x = return $ return x

evalFuncIO :: String -> Json -> JLIO ResultIO
evalFuncIO fName param = do
  ops <- getOperationsIO
  vars <- getVariablesIO
  function <- getFunctionIO fName
  return $ case function of
    Nothing -> return $ throwError $ "Function: " ++ fName ++ " not found"
    Just f -> f (subEvalIO ops) param vars

subEvalIO :: M.Map String FunctionIO -> Rule -> Data -> ResultIO
subEvalIO ops rule d = runReader (evalRuleIO rule) $ JLEnvIO ops d
