module JsonLogic.Utils where

import Data.List ((!!))
import Data.Map as M (lookup)
import JsonLogic.Json

-- Splits string on periods
-- Same definition as words at: https://github.com/ghc/ghc/blob/master/libraries/base/Data/OldList.hs
-- >>> splitOnPeriod "foo.bar.tea"
-- ["foo","bar","tea"]
splitOnPeriod :: String -> [String]
splitOnPeriod "" = []
splitOnPeriod s = case dropWhile ('.' Prelude.==) s of
  "." -> []
  s' -> w : splitOnPeriod s''
    where
      (w, s'') = break ('.' Prelude.==) s'

-- Indexing an array. A double 0.1 is first converted to [0,1]
-- With this list we index recursively on the jsonObject or JsonString
-- If it cannot index the object then it is always JsonNull
-- >>> indexArray [] (JsonNumber 1)
-- JsonNumber 1.0
-- >>> indexArray [1] (JsonArray [JsonString "abc", JsonString "def"])
-- JsonString "def"
-- >>> indexArray [1,0] (JsonArray [JsonString "abc", JsonString "def"])
-- JsonString "d"
indexArray :: Double -> Data -> Json
indexArray d = recIndexArray (map read $ splitOnPeriod $ show d)
  where
    recIndexArray :: [Int] -> Data -> Json
    recIndexArray [] vars = vars
    recIndexArray (x : xs) (JsonArray js)
      | x >= 0 && x < length js = recIndexArray xs (js !! x)
      | otherwise = JsonNull
    recIndexArray [x] (JsonString s)
      | x >= 0 && x < length s = JsonString [s !! x]
      | otherwise = JsonNull
    recIndexArray _ _ = JsonNull

-- Indexes a Json object using string.
-- Only indexing a json object is possible with strings
-- Other json objects cant be indexed.
-- >>> indexData "" (JsonNumber 1)
-- JsonNumber 1.0
indexData :: String -> Data -> Json
indexData s = recIndexData (splitOnPeriod s)
  where
    recIndexData :: [String] -> Data -> Json
    recIndexData [] vars = vars
    recIndexData (x : xs) (JsonObject o) = case M.lookup x o of
      Nothing -> JsonNull -- If member is not present it returns Null
      Just js -> recIndexData xs js
    recIndexData _ _ = JsonNull

-- Default is only given if the initial object is an array
getJsonWithDefault :: Json -> (Json, Json)
getJsonWithDefault (JsonArray [x, y]) = (x, y)
getJsonWithDefault j = (j, JsonNull)

-- If first is Null then return default
returnOrDefault :: Json -> Json -> Json
returnOrDefault JsonNull j2 = j2
returnOrDefault j1 _ = j1
