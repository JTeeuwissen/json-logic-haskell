module JsonLogic.Operation.Utils where

import qualified Data.Map as M (lookup)
import JsonLogic.Json (Data, Json (..), Rule)
import Text.Read (readMaybe)

-- | Index a json object using a string seperated by periods.
-- >>> indexJson "x.y" (JsonObject $ singleton "x" $ JsonObject $ singleton "y" JsonNull)
-- Just JsonNull
-- >>> indexJson "x.y" (JsonObject $ singleton "x" JsonNull)
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
  readMaybe x >>= (!?) s >>= Just . JsonString . singleton
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

-- | Returns the single item in a list if the argument is an array, otherwise returns the argument
-- If you like, we support syntactic sugar to skip the array around single arguments
-- Should only be used for unary operations.
-- >>> evaluateUnaryArgument $ JsonArray [JsonString "abc"]
-- JsonString "abc"
-- >>> evaluateUnaryArgument $ JsonString "abc"
-- JsonString "abc"
evaluateUnaryArgument :: Data -> Data
evaluateUnaryArgument (JsonArray [json]) = json
evaluateUnaryArgument json = json 

-- | Put a single item in a list
-- Included in base since: base-4.15.0.0
-- But currently on older version.
-- >>> singleton "single value"
-- ["single value"]
singleton :: a -> [a]
singleton x = [x]
