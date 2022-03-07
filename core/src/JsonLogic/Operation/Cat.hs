module JsonLogic.Operation.Cat where

import JsonLogic.Json (Function, Json (JsonArray, JsonString), stringify)
import JsonLogic.Operation.Primitive

evaluateCat :: Function
evaluateCat evaluator args vars = do
  res <- evaluator args vars
  case res of
    (JsonArray js) -> return $ JsonString $ foldMap stringify js
    json -> return $ JsonString $ stringify json
