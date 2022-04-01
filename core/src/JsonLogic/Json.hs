module JsonLogic.Json where

import Control.Applicative
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read

-- | Json is a collection of possible JSON values.
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject JsonObject
  deriving (Eq)

-- | A Json object is a collection of key-value pairs.
type JsonObject = M.Map String Json

-- | A rule can be any kind of JSON value, but objects and arrays will be evaluated.
type Rule = Json

-- | Data can be any kind of JSON value.
type Data = Json

-- | An instance to show json in clear format for users
instance Show Json where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber d) = show d
  show (JsonString s) = show s
  show (JsonArray js) = show js
  show (JsonObject o) = "{" ++ intercalate "," (map (\(k, v) -> show k ++ ":" ++ show v) $ M.toList o) ++ "}"

instance Read Json where
  readPrec = parens readValue

-- | A pretty formatted show for the json, with identation and depth
-- Use putStr so the newline characters will be interpreted in console
-- >>> putStr $ prettyShow JsonNull
-- null
-- >>> putStr $ prettyShow $ JsonNumber 3.0
-- 3.0
-- >>> prettyShow (JsonArray [JsonNumber 1, JsonNumber 2])
-- "[\n  1.0,\n  2.0\n]"
-- >>> putStr $ prettyShow (JsonArray [JsonNumber 1, JsonBool True])
-- [
--   1.0,
--   true
-- ]
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
        ++ commaSeparate (map (\(k, v) -> tab nrSpaces ++ show k ++ ": " ++ prettyShow' (nrSpaces + 2) v) $ M.toList o)
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
-- Same as the Parsefloat function in JS
-- Parsefloat source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseFloat
-- NaN source: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
parseFloat :: Json -> Double
-- Numbers stay just numbers
parseFloat (JsonNumber n) = n
-- The string "Infinity" is parsed as actual infinity
parseFloat (JsonString "Infinity") = infinity
-- First drop all whitespace, then take all "valid" characters. Drop everything after the second point and then try to parse it to a double.
parseFloat (JsonString s) = fromMaybe notANumber $ readMaybe $ dropAfterSecondPoint $ takeWhile isValid $ dropWhile isSpace s
  where
    -- Numbers, decimal point, +/- for sign and e/E for exponent are valid characters.
    isValid x
      | x `elem` valids = True
      | otherwise = False
    valids = ['0' .. '9'] ++ ['.', 'e', 'E', '+', '-']
    -- Break on the first decimal point, then the second, and glue the first parts together to drop evertyhing after the second point.
    dropAfterSecondPoint t = case break (== '.') t of
      (l, '.' : r) -> case break (== '.') r of (l', _) -> l ++ "." ++ l'
      (l, _) -> l
-- For an array always take the first element.
parseFloat (JsonArray (a : _)) = parseFloat a
-- Everything else is NaN
parseFloat _ = notANumber

-- | Gives a Infinity
infinity :: Double
infinity = 1 / 0

-- | Gives a NaN
notANumber :: Double
notANumber = 0 / 0

-- Parsing
-- See https://www.json.org/json-en.html

readObject :: ReadPrec JsonObject
readObject = do
  Char '{' <- lexP
  xs <-
    ( do
        readWhitespace
        return []
      )
      +++ ( do
              h <- readKvp
              t <- many $ do
                Char ',' <- lexP
                readKvp
              return $ h : t
          )
  Char '}' <- lexP
  return $ M.fromList xs
  where
    readKvp = do
      readWhitespace
      key <- readString
      readWhitespace
      Char ':' <- lexP
      value <- readValue
      return (key, value)

readArray :: ReadPrec [Json]
readArray = do
  Char '[' <- lexP
  xs <-
    ( do
        readWhitespace
        return []
      )
      +++ ( do
              h <- readValue
              t <- many $ do
                Char ',' <- lexP
                readValue
              return $ h : t
          )
  Char ']' <- lexP
  return xs

readValue :: ReadPrec Json
readValue = do
  readWhitespace
  v <-
    ( JsonString <$> readString
      )
      +++ ( JsonNumber <$> readNumber
          )
      +++ ( JsonObject <$> readObject
          )
      +++ ( JsonArray <$> readArray
          )
      +++ ( do
              Symbol "true" <- lexP
              return $ JsonBool True
          )
      +++ ( do
              Symbol "false" <- lexP
              return $ JsonBool False
          )
      +++ ( do
              Ident "null" <- lexP
              return JsonNull
          )
  readWhitespace
  return v

readString :: ReadPrec String
readString = do
  Char '\"' <- lexP
  xs <- many $ do
    undefined -- TODO
  Char '\"' <- lexP
  return xs

readNumber :: ReadPrec Double
readNumber = undefined -- TODO

readWhitespace :: ReadPrec ()
readWhitespace =
  ()
    <$ many
      ( ( do
            Symbol " " <- lexP
            readWhitespace
        )
          +++ ( do
                  Symbol "\n" <- lexP
                  readWhitespace
              )
          +++ ( do
                  Symbol "\t" <- lexP
                  readWhitespace
              )
          +++ ( do
                  Symbol "\r" <- lexP
                  readWhitespace
              )
      )
