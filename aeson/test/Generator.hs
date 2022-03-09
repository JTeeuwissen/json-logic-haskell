module Generator where

import Data.List (intercalate, nubBy)
import qualified Data.Map as M (fromList)
import Hedgehog
import Hedgehog.Gen (alphaNum, bool, choice, double, int, string)
import Hedgehog.Range as Range (constant)
import JsonLogic.Json (Json (..))

genNull :: Gen (Json, String)
genNull = return (JsonNull, "null")

genBool :: Gen (Json, String)
genBool = do
  b <- bool
  return $
    if b
      then (JsonBool b, "true")
      else (JsonBool b, "false")

genNumber :: Gen (Json, String)
genNumber = do
  d <- double $ Range.constant 0 100
  return (JsonNumber d, show d)

genString :: Gen (Json, String)
genString = do
  s <- string (Range.constant 0 10) alphaNum
  return (JsonString s, show s)

genArray :: Size -> Gen (Json, String)
genArray size = do
  sizes <- genUnbalancedSizeList size
  js <- mapM genJson sizes
  return (JsonArray $ map fst js, "[" <> intercalate "," (map snd js) <> "]")

genEntry :: Size -> Gen ((String, Json), String)
genEntry size = do
  key <- string (Range.constant 1 10) alphaNum
  (json, str) <- genJson size
  return ((key, json), show key <> ":" <> str)

genObject :: Size -> Gen (Json, String)
genObject size = do
  sizes <- genUnbalancedSizeList size
  js <- mapM genEntry sizes
  -- Only keep the unique key values to prevent unexpected behavior
  let js' = nubBy (\j1 j2 -> fst (fst j1) == fst (fst j2)) js
  return (JsonObject $ M.fromList $ map fst js', "{" <> intercalate "," (map snd js') <> "}")

genJson :: Size -> Gen (Json, String)
genJson s@(Size size)
  -- If size less or equal to 0 a final item is closed
  | size <= 0 =
    choice
      [ genNull,
        genBool,
        genNumber,
        genString
      ]
  -- If size is greater than 0 we expand with an array or object
  | otherwise =
    choice
      [ genArray s,
        genObject s
      ]

-- | Generates a list of uneven sizes
genUnbalancedSizeList :: Size -> Gen [Size]
genUnbalancedSizeList (Size size) = do
  -- On average contain ~5 items
  arrayLength <- int $ Range.constant 1 10
  let elementSize = size `div` arrayLength
  (Size <$>) <$> genUnbalancedIntList size elementSize

-- | Create an unequal list of integers
genUnbalancedIntList ::
  -- | How many items are still remaining to get divided
  Int ->
  -- | The maximum size of a single entry in the list
  Int ->
  -- | A randomly generated list of integers
  Gen [Int]
genUnbalancedIntList remaining maxInt
  | remaining <= 0 = return []
  | otherwise = do
    chunkSize <- int $ Range.constant 0 $ min remaining maxInt
    -- Important! -1 so the size of each element will converge to 0 eventually
    (:) chunkSize <$> genUnbalancedIntList (remaining - chunkSize - 1) maxInt
