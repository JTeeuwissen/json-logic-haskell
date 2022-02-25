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

-- | Generator for empty strings
genGenericEmptyJsonString :: Gen (Json, String)
genGenericEmptyJsonString = return (JsonString "", "")

-- | Generator for a random JsonString
genGenericJsonString :: Gen (Json, String)
genGenericJsonString = do
  s <- string (Range.constantFrom 1 3 10) alphaNum
  return (JsonString s, s)

-- | Generator for empty JsonArrays
genGenericEmptyJsonArray :: Gen (Json, [a])
genGenericEmptyJsonArray = return (JsonArray [], [])
