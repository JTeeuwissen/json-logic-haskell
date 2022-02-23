module Generator.Generic where

import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range
import JsonLogic.Json

genGenericJsonNull :: Gen (Json, Json)
genGenericJsonNull = return (JsonNull, JsonNull)

genGenericJsonBool :: Gen (Json, Bool)
genGenericJsonBool = do
  b <- bool
  return (JsonBool b, b)

genGenericJsonNumber :: Gen (Json, Double)
genGenericJsonNumber = do
  d <- double $ Range.constantFrom 1 10 100
  return (JsonNumber d, d)

genGenericEmptyJsonString :: Gen (Json, String)
genGenericEmptyJsonString = return (JsonString "", "")

genGenericJsonString :: Gen (Json, String)
genGenericJsonString = do
  s <- string (Range.constantFrom 1 3 20) alphaNum
  return (JsonString s, s)

genGenericEmptyJsonArray :: Gen (Json, [a])
genGenericEmptyJsonArray = return (JsonArray [], [])

-- genGenericJsonObject :: Gen (Json, M.Map String Json)
-- genGenericJsonObject = undefined
