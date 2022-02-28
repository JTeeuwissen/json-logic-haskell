module JsonLogic.Operation.Utils where

import qualified Data.List as L (singleton)
import qualified Data.Map as M (lookup)
import JsonLogic.Json (Data, Json (..))
import Text.Read (readMaybe)

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
indexJson :: String -> Data -> Maybe Json
indexJson indexString = index (splitOnPeriod indexString)
  where
    index :: [String] -> Json -> Maybe Json
    index [] vars = Just vars
    index [x] (JsonString s) =
      readMaybe x >>= (!?) s >>= Just . JsonString . L.singleton
    index (x : xs) (JsonArray js) =
      readMaybe x >>= (!?) js >>= index xs
    index (x : xs) (JsonObject o) = M.lookup x o >>= index xs
    index _ _ = Nothing

-- Safe indexing of a list
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)
