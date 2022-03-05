module JsonLogic.Operation.Cat where

import Data.List (intercalate)
import JsonLogic.Json (Function, Json (JsonArray, JsonObject, JsonString))

evaluateCat :: Function
evaluateCat evaluator args vars = do
  res <- evaluator args vars
  return $ JsonString $ foldShowJson res

foldShowJson :: Json -> String
foldShowJson (JsonArray js) = foldMap foldShowJson' js
foldShowJson json = show json

foldShowJson' :: Json -> String
foldShowJson' (JsonArray js) = intercalate "," $ map show js
foldShowJson' json = show json
