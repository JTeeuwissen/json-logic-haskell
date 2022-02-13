module Json (Json (JsonNull, JsonBool, JsonNumber, JsonString, JsonArray, JsonObject)) where

import qualified Data.Map as Map

data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject (Map.Map String Json)
  deriving (Eq, Show)
