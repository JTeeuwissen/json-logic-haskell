{-# LANGUAGE OverloadedLists #-}

module Generator.Data where

import Data.Map as M (fromList, insert)
import Generator.Generic
  ( genGenericJsonBool,
    genGenericJsonNumber,
    genGenericJsonString,
    genGenericNonEmptyJsonString,
  )
import Generator.Utils (genUnbalancedSizeList)
import Hedgehog (Gen, Size (Size))
import Hedgehog.Gen (choice)
import JsonLogic.Json (Json (..))
import Text.Read (readMaybe)

-- | Inserts Json into a specific path and returns it
-- If it is arrived at the end of the path then it gives the value back
insertAtPath ::
  -- | The path at which to store the json
  [String] ->
  -- | Json data that needs to get inserted
  Json ->
  -- | Json object that needs to get updated
  Json ->
  -- | Updated Json object with inserted Json
  Json
-- End of path reached
insertAtPath [] value _ = value
-- Insert value to the map
insertAtPath (p : ps) value (JsonObject o) = case readMaybe p of
  Nothing -> JsonObject $ insert p (insertAtPath ps value JsonNull) o
  Just i -> JsonArray $ replicate (i :: Int) JsonNull ++ [insertAtPath ps value JsonNull]
-- Insert value into an array
insertAtPath (p : ps) value (JsonArray js) = case readMaybe p of
  Nothing -> JsonObject [(p, insertAtPath ps value JsonNull)]
  -- Insert it into array if it already has the length
  Just i
    | i < length js ->
      let (xs, ys) = splitAt i js
       in -- Replacing the index with the new item, for this we need to drop 1 element at the end
          JsonArray $ xs ++ [insertAtPath ps value JsonNull] ++ drop 1 ys
    -- Otherwise append items to the list and put it at the end
    | otherwise -> JsonArray $ js ++ replicate ((i :: Int) - length js) JsonNull ++ [insertAtPath ps value JsonNull]
-- It is inserting along a new path, denoted with JsonNull
insertAtPath (p : ps) value JsonNull = case readMaybe p of
  Nothing -> JsonObject [(p, insertAtPath ps value JsonNull)]
  Just i -> JsonArray $ replicate (i :: Int) JsonNull ++ [insertAtPath ps value JsonNull]
-- Data is always an array or an object in the top layer, everything else is wrong
insertAtPath _ _ _ = error "Error invalid Json, your json data is not an array or object"

-- | Generate random Json given a size
genSizedRandomJson :: Size -> Gen Json
genSizedRandomJson s@(Size size)
  -- If size less or equal to 0 a final item is closed
  | size <= 0 =
    choice
      [ return JsonNull,
        fst <$> genGenericJsonBool,
        fst <$> genGenericJsonNumber,
        fst <$> genGenericJsonString
      ]
  -- If size is greater than 0 we expand with an array or object
  | otherwise =
    choice
      [ genSizedRandomJsonArray s,
        genSizedRandomJsonObject s
      ]

-- | Generate a Random sized Json array
genSizedRandomJsonArray :: Size -> Gen Json
genSizedRandomJsonArray size = do
  sizes <- genUnbalancedSizeList size
  JsonArray <$> mapM genSizedRandomJson sizes

-- | Generate a Random size Json array that does not contain any objects
genSizedNestedJsonArray :: Size -> Gen Json
genSizedNestedJsonArray size
  | size <= 0 =
    choice
      [ return JsonNull,
        fst <$> genGenericJsonBool,
        fst <$> genGenericJsonNumber,
        fst <$> genGenericJsonString
      ]
  | otherwise = do
    sizes <- genUnbalancedSizeList size
    JsonArray <$> mapM genSizedNestedJsonArray sizes

-- | Generate sized Jsonobject entry (pair<key,value>)
genSizedRandomJsonEntry :: Size -> Gen (String, Json)
genSizedRandomJsonEntry size = do
  str <- snd <$> genGenericNonEmptyJsonString
  json <- genSizedRandomJson size
  return (str, json)

-- | Generate random Json object with a given size
genSizedRandomJsonObject :: Size -> Gen Json
genSizedRandomJsonObject size = do
  sizes <- genUnbalancedSizeList size
  JsonObject . M.fromList <$> mapM genSizedRandomJsonEntry sizes

-- | Generate an array of given size that generates a range array
genSizedJsonNumberArray :: Size -> Gen (Json, [Double])
genSizedJsonNumberArray (Size size) = do
  let arr = [1 .. (1.0 + fromIntegral size)] :: [Double]
  return (JsonArray $ map JsonNumber arr, arr)

-- | Generate a flat array of a given size
genSizedFlatArray :: Size -> Gen Json
genSizedFlatArray (Size size) = JsonArray <$> mapM (\_ -> genSizedRandomJson $ Size 0) [0 .. size]
