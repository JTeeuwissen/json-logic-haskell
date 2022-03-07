module JsonLogic.Operation.Merge (evaluateMerge) where

import JsonLogic.Json (Function, Json (JsonArray))

-- | Merge operations flattens the array in the top level
evaluateMerge :: Function
evaluateMerge evaluator params vars = do
  res <- evaluator params vars
  case res of
    (JsonArray js) -> return $ JsonArray $ foldr merge [] js
    -- If we get a single item, it gets put in an array
    json -> return $ JsonArray [json]
  where
    merge (JsonArray as) acc = as ++ acc
    merge j acc = j : acc
