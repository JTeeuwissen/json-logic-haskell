module Generator.Utils where

import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range
import JsonLogic.Json

genUnbalancedSizes :: Size -> Gen (Size, Size)
genUnbalancedSizes (Size size) = do
  balanceOffset <- int $ Range.constant (-size) size
  let s1 = Size $ (size + balanceOffset) `div` 2
      s2 = Size $ (size - balanceOffset) `div` 2
  return (s1, s2)
