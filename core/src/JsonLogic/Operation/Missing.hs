module JsonLogic.Operation.Missing where

import Control.Monad.Except (MonadError (throwError))
import Data.Maybe (isNothing)
import JsonLogic.Json (Function, Json (..))
import JsonLogic.Operation.Utils

-- | Evaluates which elements are missing from the Json
evaluateMissing :: Function
evaluateMissing evaluator param vars = do
  res <- evaluator param vars
  -- Only keep the missing values in the json array
  return $ JsonArray [a | a <- keys res, isNothing $ indexWithJson a vars]
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
  params <- evaluator y vars
  missingArr <- evaluateMissing evaluator params vars
  let (JsonArray missing) = missingArr
  case params of
    -- Return result if at least x elements are missing or else an empty array
    JsonArray js | length js - length missing >= floor x -> return $ JsonArray []
    JsonArray _ -> return missingArr
    -- If there is only a singleton as parameter, the length is 1
    _ | 1 - length missing >= floor x -> return $ JsonArray []
    _ -> return missingArr
-- The parameters are invalid
evaluateMissingSome _ json _ = throwError $ "Error: missing_some expects an array of two arguments, instead it got: " ++ show json
