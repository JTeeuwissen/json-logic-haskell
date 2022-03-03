module JsonLogic.Operation.Var (evaluateVar) where

import Data.Maybe (fromMaybe)
import JsonLogic.Json (Data, Json (..), Rule, SubEvaluator)
import JsonLogic.Operation.Utils

-- Evaluates a var
evaluateVar :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateVar evaluator param vars = do
  res <- evaluator param vars
  -- Extracts default value from array if it has one
  let (j, def) = getJsonWithDefault res
  case j of
    -- Indexing using a floored double or index object using a string
    i@(JsonNumber _) -> return $ fromMaybe def $ indexWithJson i vars
    i@(JsonString _) -> return $ fromMaybe def $ indexWithJson i vars
    -- null and empty array return the variables directly
    JsonNull -> return vars
    JsonArray [] -> return vars
    -- Nested array, boolean and object always resort to default value
    _ -> return def

-- | When var receives an array, the first item is the initial logic
-- If that logic fails then the second value is defaulted to
-- Any valuie after the second one is ignored
getJsonWithDefault :: Json -> (Json, Json)
getJsonWithDefault (JsonArray (x : y : _)) = (x, y)
getJsonWithDefault j = (j, JsonNull)
