{-# LANGUAGE OverloadedLists #-}

module Generator.Data where

import Data.Map as M (fromList, insert)
import Generator.Generic
  ( genGenericJsonBool,
    genGenericJsonNumber,
    genGenericJsonString,
  )
import Generator.Utils (genUnbalancedSizeList)
import Hedgehog (Gen, Size (Size))
import Hedgehog.Gen (choice)
import JsonLogic.Json (Json (JsonArray, JsonNull, JsonNumber, JsonObject))
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
  Just i ->
    if i < length js
      then -- Insert it into array
        JsonArray $ (\(s, e : es) -> s ++ [insertAtPath ps value JsonNull] ++ es) $ splitAt i js
      else -- Else append items to the list and put it at the end
        JsonArray $ js ++ replicate ((i :: Int) - length js) JsonNull ++ [insertAtPath ps value JsonNull]
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

-- | Generate sized Jsonobject entry (pair<key,value>)
genSizedRandomJsonEntry :: Size -> Gen (String, Json)
genSizedRandomJsonEntry size = do
  str <- snd <$> genGenericJsonString
  json <- genSizedRandomJson size
  return (str, json)

-- | Generate random Json object with a given size
genSizedRandomJsonObject :: Size -> Gen Json
genSizedRandomJsonObject size = do
  sizes <- genUnbalancedSizeList size
  JsonObject . M.fromList <$> mapM genSizedRandomJsonEntry sizes
