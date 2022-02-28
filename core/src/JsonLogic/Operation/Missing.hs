module JsonLogic.Operation.Missing where

import Control.Monad.Except (MonadError (throwError))
import Data.Maybe (mapMaybe)
import JsonLogic.Json (Data, Function, Json (..), Rule, SubEvaluator)
import JsonLogic.Operation.Utils

-- | Evaluates which elements are missing from the Json
evaluateMissing :: Function
evaluateMissing evaluator param vars = do
  res <- evaluator param vars
  return $ JsonArray $ mapMaybe (`indexWithJson` vars) $ keys res
  where
    -- The keys used for our search
    keys :: Json -> [Json]
    keys (JsonArray (JsonArray js : _)) = js
    keys (JsonArray js) = js
    keys j = [j]

-- | Evaluates whether more than x items are missing from the original array
-- If so, it returns the entire list of missing items
-- Otherwise it returns an empty list
evaluateMissingSome :: Function
evaluateMissingSome evaluator (JsonArray [JsonNumber x, y]) vars = do
  res <- evaluateMissing evaluator y vars
  case res of
    -- Return result if at least x elements are missing or else an empty array
    (JsonArray js) | jsonLength y - length js >= floor x -> return $ JsonArray []
    _ -> return res
-- The parameters are invalid
evaluateMissingSome _ json _ = throwError $ "Error: missing_some expects an array of two arguments, instead it got: " ++ show json

-- Length of the json
jsonLength :: Json -> Int
jsonLength (JsonArray js) = length js
jsonLength _ = 1
