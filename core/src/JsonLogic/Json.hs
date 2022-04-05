-- |
-- Module      : JsonLogic.Json
-- Description : JsonLogic Json object with utility functions and read/show instances
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Json (Json (..), JsonObject, Rule, Data, prettyShow, stringify, isTruthy, isFalsy, parseFloat) where

import Control.Applicative
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read
  ( Read (readPrec),
    ReadPrec,
    get,
    parens,
    pfail,
    readMaybe,
    (+++),
  )

-- | Json is a collection of possible Json values.
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject JsonObject
  deriving (Eq)

-- | A Json object is a map of string-Json pairs.
type JsonObject = M.Map String Json

-- | A rule can be any kind of Json value, but objects and arrays will be evaluated.
type Rule = Json

-- | Data can be any kind of Json value.
type Data = Json

-- An instance to show Json in clear format for users
instance Show Json where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber d) = show d
  show (JsonString s) = show s
  show (JsonArray js) = show js
  show (JsonObject o) = "{" ++ intercalate "," (map (\(k, v) -> show k ++ ":" ++ show v) $ M.toList o) ++ "}"

-- Using a custom parser to read the Json according to specification.
instance Read Json where
  readPrec = parens readValue

-- | A pretty formatted show for the Json, with identation and depth
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

-- | Convert Json to string, used in string operations
-- See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/toString for more information.
stringify :: Json -> String
stringify JsonNull = ""
stringify (JsonBool True) = "true"
stringify (JsonBool False) = "false"
stringify (JsonNumber d) = show d
stringify (JsonString s) = s
stringify (JsonArray js) = intercalate "," $ map stringify js
stringify (JsonObject _) = "[object Object]"

-- | Truthy test for Json
-- See https://developer.mozilla.org/en-US/docs/Glossary/Truthy for more information.
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

-- | The opposite of `isTruthy`
isFalsy :: Json -> Bool
isFalsy = not . isTruthy

-- | Convert Json to a numeric value, including NaN
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

-- Gives a Infinity
infinity :: Double
infinity = 1 / 0

-- Gives a NaN
notANumber :: Double
notANumber = 0 / 0

-- Parsing
-- See https://www.json.org/json-en.html

-- Read an object, a map with strings as keys.
readObject :: ReadPrec JsonObject
readObject = do
  '{' <- get
  items <-
    ( do
        readWhitespace
        return []
      )
      +++ ( do
              h <- readKvp
              t <- many $ do
                ',' <- get
                readKvp
              return $ h : t
          )
  '}' <- get
  return $ M.fromList items
  where
    readKvp = do
      readWhitespace
      key <- readString
      readWhitespace
      ':' <- get
      value <- readValue
      return (key, value)

-- Read an array, can contain multiple comma separated values.
readArray :: ReadPrec [Json]
readArray = do
  '[' <- get
  items <-
    ( do
        readWhitespace
        return []
      )
      +++ ( do
              h <- readValue
              t <- many $ do
                ',' <- get
                readValue
              return $ h : t
          )
  ']' <- get
  return items

-- Read a value, wrapper around many of the other parsers.
readValue :: ReadPrec Json
readValue = do
  readWhitespace
  value <-
    (JsonString <$> readString)
      +++ (JsonNumber <$> readNumber)
      +++ (JsonObject <$> readObject)
      +++ (JsonArray <$> readArray)
      +++ ( do
              "true" <- many get
              return $ JsonBool True
          )
      +++ ( do
              "false" <- many get
              return $ JsonBool False
          )
      +++ ( do
              "null" <- many get
              return JsonNull
          )
  readWhitespace
  return value

-- Reads a string with escaping.
readString :: ReadPrec String
readString = do
  '\"' <- get
  xs <-
    many $
      ( do
          char <- get
          case char of
            '\\' -> pfail
            '\"' -> pfail
            plain -> return plain
      )
        +++ ( do
                '\\' <- get
                char <- get
                case char of
                  '\"' -> return '\"'
                  '\\' -> return '\\'
                  '/' -> return '/'
                  'b' -> return '\b'
                  'f' -> return '\f'
                  'n' -> return '\n'
                  'r' -> return '\r'
                  't' -> return '\t'
                  'u' -> do
                    a <- readHex
                    b <- readHex
                    c <- readHex
                    d <- readHex
                    return $ toEnum $ foldl (\l r -> l * 16 + r) 0 [a, b, c, d]
                  _ -> pfail
            )
  '\"' <- get
  return xs
  where
    readHex = readMap $ zip (['0' .. '9'] ++ ['A' .. 'F'] ++ ['a' .. 'f']) ([0 .. 9] ++ [10 .. 15] ++ [10 .. 15])

-- Reads a number, including the sign and exponent as a double.
readNumber :: ReadPrec Double
readNumber = do
  -- An optional negative sign
  sign <-
    return id
      +++ ( do
              '-' <- get
              return negate
          )
  -- Numbers before the optional decimal.
  beforeDecimal <-
    ( -- Can be just 0
      do
        '0' <- get
        return 0
      )
      +++ (
            -- Or can be non zero, without starting with a 0
            do
              nonZeroDigit <- getNonZeroDigit
              digits <- many getDigit
              return $ foldl (\l r -> l * 10 + r) nonZeroDigit digits
          )
  -- Numbers after the optional decimal
  afterDecimal <-
    return id
      +++ ( do
              -- After decimal single or more digits
              '.' <- get
              digits <- some getDigit
              -- Added 0 to the front of the digits to make it below 1
              return $ (+) $ foldr1 (\l r -> l + r / 10) (0 : digits)
          )
  -- Or zero if no decimal
  -- The number exponent
  expo <-
    -- Can be 1 if no exponent.
    return id +++ do
      -- Otherwise may start with e or E
      ( do
          'e' <- get
          return ()
        )
        +++ ( do
                'E' <- get
                return ()
            )
      -- Then may have a sign, defaulting to positive
      expBase <-
        return (*)
          +++ ( do
                  '-' <- get
                  return $ flip (/)
              )
          +++ ( do
                  '+' <- get
                  return (*)
              )
      -- Then some digits determining the size.
      expDigits <-
        ( do
            digits <- some getDigit
            return $ foldl1 (\l r -> l * 10 + r) digits
          )
      return $ expBase $ 10 ** expDigits
  -- Then combine everything.
  return $ sign $ expo $ afterDecimal beforeDecimal
  where
    getDigit = readMap $ zip ['0' .. '9'] [0 .. 9]
    getNonZeroDigit = readMap $ zip ['1' .. '9'] [1 .. 9]

-- Reads whitespace and throws it away.
readWhitespace :: ReadPrec ()
readWhitespace = () <$ many (readMap $ zip " \t\n\r" (repeat ()))

-- Use a lookup table to parse characters.
readMap :: [(Char, a)] -> ReadPrec a
readMap xs = do
  x <- get
  maybe pfail return (lookup x xs)
