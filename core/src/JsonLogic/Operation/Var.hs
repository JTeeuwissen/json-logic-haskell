module JsonLogic.Operation.Var (evaluateVar) where

import Control.Monad.Except (MonadError (throwError))
import Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import JsonLogic.Json (Data, Json (..), Rule, SubEvaluator)
import Text.Read (readMaybe)

-- Evaluates a var
evaluateVar :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateVar evaluator param vars = do
  res <- evaluator param vars
  -- Extracts default value from array if it is one
  let (j, def) = getJsonWithDefault res
  case j of
    -- Always: JsonNull -> vars and JsonBool -> Default value
    JsonNull -> return vars
    JsonBool _ -> return def
    -- Indexing using a floored double
    JsonNumber n -> return $ fromMaybe def $ indexJson (show (floor n :: Int)) vars
    JsonString s -> return $ fromMaybe def $ indexJson s vars
    -- Default value is already extracted, cannot have nested list as var value
    JsonArray js -> throwError $ "Cannot evaluate a var of type array, namely: " ++ show js
    JsonObject o -> throwError $ "Cannot evaluate a var of type object, namely: " ++ show o

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

-- Index a json object using a string seperated by periods.
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
      readMaybe x >>= \i ->
        if i >= 0 && i <= length s
          then Just $ JsonString [s !! i]
          else Nothing
    index (x : xs) (JsonArray js) =
      readMaybe x >>= \i ->
        if i >= 0 && i <= length js
          then index xs (js !! i)
          else Nothing
    index (x : xs) (JsonObject o) = case M.lookup x o of
      Nothing -> Nothing -- If member is not present it returns Null
      Just js -> index xs js
    index _ _ = Nothing

-- Default is only given if the initial object is an array
getJsonWithDefault :: Json -> (Json, Json)
getJsonWithDefault (JsonArray [x, y]) = (x, y)
getJsonWithDefault j = (j, JsonNull)
