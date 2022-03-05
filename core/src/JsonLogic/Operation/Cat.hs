module JsonLogic.Operation.Cat where

import Data.List (intercalate)
import JsonLogic.Json (Function, Json (JsonArray, JsonString))

evaluateCat :: Function
evaluateCat evaluator args vars = do
  res <- evaluator args vars
  return $ JsonString $ foldShowJson res

foldShowJson :: Json -> String
foldShowJson (JsonArray js) = foldMap foldNestedJson js
  where
    -- A nested array does intercalate commas, they stay in the final result
    foldNestedJson (JsonArray xs) = intercalate "," $ map show xs
    foldNestedJson json = show json
foldShowJson json = show json
