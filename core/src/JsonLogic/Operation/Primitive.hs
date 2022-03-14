module JsonLogic.Operation.Primitive where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.Json
import JsonLogic.Type

-- Primitive evaluators
evaluateDouble :: SubEvaluator -> Rule -> Data -> Either String Double
evaluateDouble evaluator param vars = do
  res <- evaluator param vars
  return $ parseFloat res

evaluateInt :: SubEvaluator -> Rule -> Data -> Either String Int
evaluateInt evaluator param vars = do
  res <- evaluateDouble evaluator param vars
  if isNaN res
    then throwError "NotImplemented: NaN to int evaluation"
    else return $ floor res

evaluateBool :: SubEvaluator -> Rule -> Data -> Either String Bool
evaluateBool evaluator param vars = do
  res <- evaluator param vars
  return $ isTruthy res

evaluateArray :: SubEvaluator -> Rule -> Data -> Either String [Json]
evaluateArray evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonArray xs -> return xs
    j -> throwError $ "Invalid parameter type, was expecting array. Got: " ++ show j

evaluateObject :: SubEvaluator -> Rule -> Data -> Either String (M.Map String Json)
evaluateObject evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonObject v -> return v
    j -> throwError $ "Invalid parameter type, was expecting object. Got: " ++ show j

evaluateString :: SubEvaluator -> Rule -> Data -> Either String String
evaluateString evaluator param vars = do
  res <- evaluator param vars
  return $ stringify res
