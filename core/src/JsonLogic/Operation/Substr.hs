module JsonLogic.Operation.Substr where

import JsonLogic.Json (Function, Json (JsonArray, JsonString), stringify)
import JsonLogic.Operation.Primitive

-- | Evaluate substr operation
evaluateSubstr :: Function
evaluateSubstr evaluator param vars = do
  res <- evaluator param vars
  JsonString <$> case res of
    -- Take everything from the index (can be negative)
    JsonArray [s, i] -> do
      str <- evaluateString evaluator s vars
      index <- evaluateInt evaluator i vars
      return $ alterSubstr drop index str
    -- Take a part of the substring between the two indexes
    JsonArray (s : startI : endI : _) -> do
      str <- evaluateString evaluator s vars
      startIndex <- evaluateInt evaluator startI vars
      endIndex <- evaluateInt evaluator endI vars
      return $ alterSubstr take endIndex $ alterSubstr drop startIndex str
    -- No proper indexing arguments given, return the full json string
    json -> evaluateString evaluator json vars
  where
    -- Takes part of the substring given a positive or negative index
    alterSubstr :: (Int -> String -> String) -> Int -> String -> String
    alterSubstr f index str
      | index >= 0 = f index str
      | otherwise = f (length str + index) str
