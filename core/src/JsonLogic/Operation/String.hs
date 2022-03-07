module JsonLogic.Operation.String where

import JsonLogic.Json (Function, Json (JsonArray, JsonString), stringify)
import JsonLogic.Operation.Primitive

evaluateCat :: Function
evaluateCat evaluator args vars = do
  res <- evaluator args vars
  case res of
    (JsonArray js) -> return $ JsonString $ foldMap stringify js
    json -> return $ JsonString $ stringify json

-- | Evaluate substr operation
evaluateSubstr :: Function
evaluateSubstr evaluator param vars = do
  res <- evaluator param vars
  case res of
    -- Take everything from the index (can be negative)
    JsonArray [s, i] -> do
      str <- evaluateString evaluator s vars
      index <- evaluateNumber evaluator i vars
      return $ JsonString $ alterSubstr drop index str
    -- Take a part of the substring between the two indexes
    JsonArray (s : startI : endI : _) -> do
      str <- evaluateString evaluator s vars
      startIndex <- evaluateNumber evaluator startI vars
      endIndex <- evaluateNumber evaluator endI vars
      return $ JsonString $ alterSubstr take endIndex $ alterSubstr drop startIndex str
    -- No proper indexing arguments given, return the full json string
    json -> do
      str <- evaluateString evaluator json vars
      return $ JsonString str
  where
    -- Takes part of the substring given a positive or negative index
    alterSubstr :: (Int -> String -> String) -> Double -> String -> String
    alterSubstr f index str
      | index >= 0 = f (floor index) str
      | otherwise = f (length str + floor index) str
