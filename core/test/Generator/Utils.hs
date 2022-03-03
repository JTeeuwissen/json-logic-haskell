module Generator.Utils where

import Hedgehog
import Hedgehog.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range as Range

-- | Splits size object into two uneven sizes
genUnbalancedSizes :: Size -> Gen (Size, Size)
genUnbalancedSizes (Size size) = do
  balanceOffset <- int $ Range.constant (-size) size
  let s1 = Size $ (size + balanceOffset) `div` 2
      s2 = Size $ (size - balanceOffset) `div` 2
  return (s1, s2)

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

increaseSizeBy :: Int -> Gen a -> Gen a
increaseSizeBy i = Gen.scale (\(Size s) -> Size $ s + i)
