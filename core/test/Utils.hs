module Utils where

import qualified Data.Map as M (Map)
import JsonLogic.Json (Json (..))

jNull :: Json
jNull = JsonNull

jBool :: Bool -> Json
jBool = JsonBool

jNum :: Double -> Json
jNum = JsonNumber

jStr :: String -> Json
jStr = JsonString

jArr :: [Json] -> Json
jArr = JsonArray

jObj :: M.Map String Json -> Json
jObj = JsonObject
