module JsonExamples where

import Data.Map as Map

import Json

-- {"log": "apple"}
logJson :: Json
logJson = JsonObject $ Map.singleton "log" (JsonString "apple")

-- { "customPlus": [1, 2] }
customPlusJson :: Json
customPlusJson = JsonObject $ Map.singleton "customOperation" (JsonArray [JsonNumber 1, JsonNumber 2])

-- { "+": [1, 2] }
plusJson :: Json
plusJson = JsonObject $ Map.singleton "+" (JsonArray [JsonNumber 1, JsonNumber 2])

-- [ true, false, true ]
listJson :: [Json]
listJson = [JsonBool True, JsonBool False, JsonBool True]

-- 'custom operator'
customOperation :: [Json] -> Maybe Json
customOperation [JsonNumber x, JsonNumber y] = Just $ JsonNumber $ x - y
customOperation _ = Nothing
