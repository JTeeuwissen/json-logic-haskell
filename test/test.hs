import Data.List
import Data.Ord
import JsonLogic
import Test.Tasty
import Test.Tasty.HUnit as H

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Validate tuple" $
        flip3 (1, 2, 3) @?= (3, 2, 1),
      -- the following test does not hold
      testCase "Validate tuple fail" $
        H.assertBool "Triplet unequal" $ flip3 (1, 2, 3) /= (1, 3, 2)
    ]