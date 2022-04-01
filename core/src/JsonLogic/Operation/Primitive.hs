module JsonLogic.Operation.Primitive where

import Control.Monad.Except
import JsonLogic.Json
import JsonLogic.Type

-- Primitive evaluators
evaluateDouble :: Monad m => SubEvaluator m -> Rule -> Data -> ExceptT String m Double
evaluateDouble evaluator param vars = do
  res <- evaluator param vars
  return $ parseFloat res

evaluateInt :: Monad m => SubEvaluator m -> Rule -> Data -> ExceptT String m Int
evaluateInt evaluator param vars = do
  res <- evaluateDouble evaluator param vars
  if isNaN res
    then throwError "NotImplemented: NaN to int evaluation"
    else return $ floor res

evaluateBool :: Monad m => SubEvaluator m -> Rule -> Data -> ExceptT String m Bool
evaluateBool evaluator param vars = do
  res <- evaluator param vars
  return $ isTruthy res

evaluateArray :: Monad m => SubEvaluator m -> Rule -> Data -> ExceptT String m [Json]
evaluateArray evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonArray xs -> return xs
    j -> throwError $ "Invalid parameter type, was expecting array. Got: " ++ show j

evaluateObject :: Monad m => SubEvaluator m -> Rule -> Data -> ExceptT String m JsonObject
evaluateObject evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonObject v -> return v
    j -> throwError $ "Invalid parameter type, was expecting object. Got: " ++ show j

evaluateString :: Monad m => SubEvaluator m -> Rule -> Data -> ExceptT String m String
evaluateString evaluator param vars = do
  res <- evaluator param vars
  return $ stringify res
