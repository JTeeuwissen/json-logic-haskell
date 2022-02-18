import Data.List
import Data.Ord
import JsonLogic
import Test.Tasty
import Test.Tasty.HUnit as H

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Succeed" $ H.assertBool "isTrue" True
    ]
