module Operation.Boolean.TestEquality where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Test.Tasty.HUnit as U
import Utils

-- See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
equalityUnitTests :: TestTree
equalityUnitTests =
  testGroup
    "Read json unit tests"
    [
       testCase "Numbers" $ assertEqual (JsonNumber 1) (JsonNumber 1) True,
       testCase "Strings" $ assertEqual (JsonString "hello") (JsonString "hello") True,
       testCase "Strings and Numbers" $ assertEqual (JsonString "1") (JsonNumber 1) True,
       testCase "Numbers and Bools" $ assertEqual (JsonNumber 0) (JsonBool False) True,
       testCase "Numbers and Null" $ assertEqual (JsonNumber 0) (JsonNull) False,
       testCase "Numbers and not not Null" $ assertEqual (JsonNumber 0) (JsonObject [("!!", JsonNull)]) True,
       testCase "Objects" $ assertEqual (JsonObject [("key", JsonString "value")]) (JsonObject [("key", JsonString "value")]) True,
    ]

assertEqual :: Json -> Json -> Bool -> Assertion
assertEqual l r b =
  U.assertEqual
    "Result is correct"
    (JsonBool b)
    (eval [] (JsonObject [("==", JsonArray [l, r])]) JsonNull)
