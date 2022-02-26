module JsonLogic where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReader)
import Data.Map as M (Map, foldr, fromList, traverseWithKey)
import JsonLogic.JL (JL, getFunction, getOperations, getVariables)
import JsonLogic.Json
  ( Data,
    Function,
    Json (JsonArray, JsonNull, JsonObject),
    JsonLogicEnv (JLEnv),
    Result,
    Rule,
  )
import JsonLogic.Operation (Operation, createEnv)

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> Result
eval ops rule d = runReader (evalRule rule) $ createEnv (M.fromList ops) d

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
evalRule :: Rule -> JL Result
evalRule o@(JsonObject rule) = do
  result <- sequenceA <$> traverseWithKey evalFunc rule
  -- An empty rule returns itself
  return $ M.foldr const o <$> result -- FIX: this only returns a single rule result. Maybe evaluate one.
evalRule (JsonArray rules) = do
  result <- sequenceA <$> mapM evalRule rules
  return $ JsonArray <$> result
evalRule x = return $ return x

evalFunc :: String -> Json -> JL Result
evalFunc fName param = do
  ops <- getOperations
  vars <- getVariables
  function <- getFunction fName
  return $ case function of
    Nothing -> throwError $ "Function: " ++ fName ++ " not found"
    Just f -> f (subEval ops) param vars

subEval :: M.Map String Function -> Rule -> Data -> Result
subEval ops rule d = runReader (evalRule rule) $ JLEnv ops d
