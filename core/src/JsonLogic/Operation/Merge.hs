module JsonLogic.Operation.Merge (evaluateMerge) where

import JsonLogic.Json (Function, Json (JsonArray))

-- | Merge operations flattens the array in the top level
evaluateMerge :: Function
evaluateMerge evaluator params vars = do
  res <- evaluator params vars
  case res of
    (JsonArray js) -> return $ JsonArray $ flattenJsonList js
    -- If we get a single item, it gets put in an array
    json -> return $ JsonArray [json]

-- | Flatten the jsonarray one level
flattenJsonList :: [Json] -> [Json]
flattenJsonList [] = []
flattenJsonList ((JsonArray as) : js) = as ++ flattenJsonList js
flattenJsonList (j : js) = j : flattenJsonList js
