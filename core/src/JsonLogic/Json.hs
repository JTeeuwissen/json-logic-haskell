module JsonLogic.Json where

import Data.List (intercalate)
import qualified Data.Map as M (Map, toList)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- A rule can be any kind of JSON value, but object will be evaluated.
type Rule = Json

-- Data can be any kind of JSON value.
type Data = Json

-- Json is a collection of possivle JSON values.
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject (M.Map String Json)
  deriving (Eq)

-- | An instance to show json in clear format for users
instance Show Json where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber d) = show d
  show (JsonString s) = show s
  show (JsonArray js) = show js
  show (JsonObject o) = "{" ++ intercalate "," (map (\(k, v) -> show k ++ ":" ++ show v) $ M.toList o) ++ "}"

-- | A pretty formatted show for the json, with identation and depth
-- print using putStrLn to interpret newline characters
prettyShow :: Json -> String
prettyShow = prettyShow' 0
  where
    -- Pretty show with the number of spaces included
    prettyShow' :: Int -> Json -> String
    prettyShow' nrSpaces (JsonArray js) =
      "[\n"
        ++ commaSeparate (map (\j -> tab nrSpaces ++ prettyShow' (nrSpaces + 2) j) js)
        ++ closingBracket nrSpaces ']'
    prettyShow' nrSpaces (JsonObject o) =
      "{\n"
        ++ commaSeparate (map (\(k, v) -> tab nrSpaces ++ show k ++ ":" ++ prettyShow' (nrSpaces + 2) v) $ M.toList o)
        ++ closingBracket nrSpaces '}'
    prettyShow' _ json = show json
    -- Helper functions for clarity
    commaSeparate :: [String] -> String
    commaSeparate = intercalate ",\n"
    closingBracket :: Int -> Char -> String
    closingBracket depth c = "\n" ++ replicate depth ' ' ++ [c]
    tab :: Int -> String
    tab depth = replicate (depth + 2) ' '

-- | Convert json to string, used in string operations
stringify :: Json -> String
stringify JsonNull = ""
stringify (JsonBool True) = "true"
stringify (JsonBool False) = "false"
stringify (JsonNumber d) = show d
stringify (JsonString s) = s
stringify (JsonArray js) = intercalate "," $ map stringify js
stringify (JsonObject _) = "[object Object]"

-- | Truthy test for json
isTruthy :: Json -> Bool
isTruthy JsonNull = False
isTruthy (JsonBool b) = b
isTruthy (JsonNumber 0.0) = False
isTruthy (JsonNumber _) = True
isTruthy (JsonString "") = False
isTruthy (JsonString _) = True
isTruthy (JsonArray []) = False
isTruthy (JsonArray _) = True
isTruthy (JsonObject _) = True

isFalsy :: Json -> Bool
isFalsy = not . isTruthy

-- | Convert json to a numeric value, including NaN
-- Same as the Number object in JS
-- Number source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number
-- NaN source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
toNumber :: Json -> Double
toNumber JsonNull = notANumber
toNumber (JsonBool True) = 1.0
toNumber (JsonBool False) = 0.0
toNumber (JsonNumber n) = n
toNumber (JsonString "") = 0.0
toNumber (JsonString s) = fromMaybe notANumber $ readMaybe s
toNumber (JsonArray []) = 0.0
toNumber (JsonArray [a]) = toNumber a
toNumber (JsonArray _) = notANumber
toNumber (JsonObject _) = notANumber

-- | Gives a NaN
notANumber :: Double
notANumber = 0 / 0

-- Subevaluator, with rule, its context and retulting json.
type SubEvaluator = Rule -> Data -> Result

type Function = SubEvaluator -> Rule -> Data -> Result

type Operations = M.Map String Function

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show (JLEnv _ vs) = "JLEnv " ++ show vs

type Result = Either String Json
