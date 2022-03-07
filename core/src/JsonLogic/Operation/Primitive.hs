module JsonLogic.Operation.Primitive where

import Control.Monad.Except (MonadError (throwError))
import JsonLogic.Json (Data, Json (..), Rule, SubEvaluator, isTruthy, stringify)

-- Primitive evaluators
evaluateDouble :: SubEvaluator -> Rule -> Data -> Either String Double
evaluateDouble evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonNumber n -> return n
    j -> throwError $ "Invalid parameter type, was expecting number. Got: " ++ show j

evaluateInt :: SubEvaluator -> Rule -> Data -> Either String Int
evaluateInt evaluator param vars = do
  res <- evaluateDouble evaluator param vars
  return $ floor res

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

evaluateString :: SubEvaluator -> Rule -> Data -> Either String String
evaluateString evaluator param vars = do
  res <- evaluator param vars
  return $ stringify res
