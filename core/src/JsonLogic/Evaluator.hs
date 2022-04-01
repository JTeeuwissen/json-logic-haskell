module JsonLogic.Evaluator where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M
import JsonLogic.JL
import JsonLogic.Json
import JsonLogic.Operation
import JsonLogic.Type

-- evaluate JsonLogic without bothering about monads
eval :: Monad m => [Operation m] -> Rule -> Data -> m (Either String Json)
eval ops rule d = runExceptT $ runReader (evalRule rule) $ createEnv (M.fromList ops) d

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
evalRule :: Monad m => Rule -> JL (Result m) m
evalRule o@(JsonObject rule) = do
  result <- sequenceA <$> traverseWithKey evalFunc rule
  -- An empty rule returns itself
  return $ M.foldr const o <$> result -- FIX: this only returns a single rule result. Maybe evaluate one.
evalRule (JsonArray rules) = do
  result <- sequenceA <$> mapM evalRule rules
  return $ JsonArray <$> result
evalRule x = return $ return x

evalFunc :: Monad m => String -> Json -> JL (Result m) m
evalFunc fName param = do
  ops <- getOperations
  vars <- getVariables
  function <- getFunction fName
  return $ case function of
    Nothing -> throwError $ "Function: " ++ fName ++ " not found"
    Just f -> f (subEval ops) param vars

subEval :: Monad m => M.Map String (Function m) -> Rule -> Data -> Result m
subEval ops rule d = runReader (evalRule rule) $ JLEnv ops d
