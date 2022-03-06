module JsonLogic.Operation.Cat where

import Data.List (intercalate)
import JsonLogic.Json (Function, Json (JsonArray, JsonString))

evaluateCat :: Function
evaluateCat evaluator args vars = do
  res <- evaluator args vars
  return $ JsonString $ foldShowJson res

foldShowJson :: Json -> String
foldShowJson (JsonArray js) = foldMap intercalateItems js
  where
    -- A nested array does intercalate commas, they stay in the final result
    intercalateItems (JsonArray xs) = intercalate "," $ map show xs
    intercalateItems json = show json
foldShowJson json = show json
