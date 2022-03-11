{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Object (objectOperations, var, missing, missingSome) where

import Control.Monad.Except
import Data.Maybe
import JsonLogic.Json
import JsonLogic.Operation.Utils

objectOperations :: Operations
objectOperations = [var, missing, missingSome]

var, missing, missingSome :: Operation
var = ("var", evaluateVar)
missing = ("missing", evaluateMissing)
missingSome = ("missing_some", evaluateMissingSome)

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
