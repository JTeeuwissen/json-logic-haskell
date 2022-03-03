module Generator.Generic where

import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range
import JsonLogic.Json

-- | Generator for a JsonNull object
genGenericJsonNull :: Gen (Json, Json)
genGenericJsonNull = return (JsonNull, JsonNull)

-- | Generator for a random JsonBool object
genGenericJsonBool :: Gen (Json, Bool)
genGenericJsonBool = do
  b <- bool
  return (JsonBool b, b)

-- | Generator for a random JsonNumber object
genGenericJsonNumber :: Gen (Json, Double)
genGenericJsonNumber = do
  d <- double $ Range.constantFrom 1 10 100
  return (JsonNumber d, d)

-- | Generator for a random JsonString
genGenericJsonString :: Gen (Json, String)
genGenericJsonString = do
  s <- string (Range.constant 0 10) alphaNum
  return (JsonString s, s)

-- | Generator for non-empty JsonStrings
genGenericNonEmptyJsonString :: Gen (Json, String)
genGenericNonEmptyJsonString = do
  s <- string (Range.constant 1 10) alphaNum
  return (JsonString s, s)

-- | Generator for empty JsonArrays
genGenericEmptyJsonArray :: Gen (Json, [a])
genGenericEmptyJsonArray = return (JsonArray [], [])
