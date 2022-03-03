module JsonLogic.Operation.Utils where

import qualified Data.List as L (singleton)
import qualified Data.Map as M (lookup)
import JsonLogic.Json (Data, Json (..), Rule)
import Text.Read (readMaybe)

-- | Index a json object using a string seperated by periods.
-- >>> indexJson "x.y" (JsonObject $ M.singleton "x" $ JsonObject $ M.singleton "y" JsonNull)
-- Just JsonNull
-- >>> indexJson "x.y" (JsonObject $ M.singleton "x" JsonNull)
-- Nothing
-- >>> indexJson "" (JsonNumber 1)
-- Just (JsonNumber 1.0)
-- >>> indexJson "1" (JsonArray [JsonString "abc", JsonString "def"])
-- Just (JsonString "def")
-- >>> indexJson "1.0" (JsonArray [JsonString "abc", JsonString "def"])
-- Just (JsonString "d")
-- >>> indexJson "abs" (JsonArray [JsonString "abc", JsonString "def"])
-- Nothing
indexWithJson :: Rule -> Data -> Maybe Json
indexWithJson (JsonString indexString) = indexWithString (splitOnPeriod indexString)
indexWithJson (JsonNumber indexNumber) = indexWithString [show (floor indexNumber :: Int)]
indexWithJson _ = const Nothing

indexWithString :: [String] -> Data -> Maybe Json
indexWithString [] vars = Just vars
indexWithString [x] (JsonString s) =
  readMaybe x >>= (!?) s >>= Just . JsonString . L.singleton
indexWithString (x : xs) (JsonArray js) =
  readMaybe x >>= (!?) js >>= indexWithString xs
indexWithString (x : xs) (JsonObject o) = M.lookup x o >>= indexWithString xs
indexWithString _ _ = Nothing

-- | Splits string on periods
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

-- Safe indexing of a list
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)
