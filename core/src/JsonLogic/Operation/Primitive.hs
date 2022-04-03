module JsonLogic.Operation.Primitive (evaluateDouble, evaluateInt, evaluateBool, evaluateArray, evaluateObject, evaluateString) where

import Control.Monad.Except
import JsonLogic.Json
import JsonLogic.Type

-- Primitive evaluators
evaluateDouble :: Monad m => Function Double m
evaluateDouble evaluator param vars = do
  res <- evaluator param vars
  return $ parseFloat res

evaluateInt :: Monad m => Function Int m
evaluateInt evaluator param vars = do
  res <- evaluateDouble evaluator param vars
  if isNaN res
    then throwError "NotImplemented: NaN to int evaluation"
    else return $ floor res

evaluateBool :: Monad m => Function Bool m
evaluateBool evaluator param vars = do
  res <- evaluator param vars
  return $ isTruthy res

evaluateArray :: Monad m => Function [Json] m
evaluateArray evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonArray xs -> return xs
    j -> throwError $ "Invalid parameter type, was expecting array. Got: " ++ show j

evaluateObject :: Monad m => Function JsonObject m
evaluateObject evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonObject v -> return v
    j -> throwError $ "Invalid parameter type, was expecting object. Got: " ++ show j

evaluateString :: Monad m => Function String m
evaluateString evaluator param vars = do
  res <- evaluator param vars
  return $ stringify res
