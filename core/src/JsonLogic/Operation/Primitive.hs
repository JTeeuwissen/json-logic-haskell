-- |
-- Module      : JsonLogic.Operation.Primitive
-- Description : Internal JsonLogic functions to evaluate to primitive types
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Operation.Primitive (evaluateDouble, evaluateDouble', evaluateInt, evaluateBool, evaluateArray, evaluateObject, evaluateString) where

import JsonLogic.Json
import JsonLogic.Type

-- Primitive evaluators
evaluateDouble :: Monad m => Function m Double
evaluateDouble evaluator param vars = do
  res <- evaluator param vars
  return $ parseFloat res

evaluateDouble' :: Monad m => Function m Double
evaluateDouble' evaluator param vars = do
  res <- evaluator param vars
  return $ parseFloat' res

evaluateInt :: Monad m => Function m Int
evaluateInt evaluator param vars = do
  res <- evaluateDouble evaluator param vars
  if isNaN res
    then throw "NotImplemented: NaN to int evaluation"
    else return $ floor res

evaluateBool :: Monad m => Function m Bool
evaluateBool evaluator param vars = do
  res <- evaluator param vars
  return $ isTruthy res

evaluateArray :: Monad m => Function m [Json]
evaluateArray evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonArray xs -> return xs
    j -> throw $ "Invalid parameter type, was expecting array. Got: " ++ show j

evaluateObject :: Monad m => Function m JsonObject
evaluateObject evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonObject v -> return v
    j -> throw $ "Invalid parameter type, was expecting object. Got: " ++ show j

evaluateString :: Monad m => Function m String
evaluateString evaluator param vars = do
  res <- evaluator param vars
  return $ stringify res
