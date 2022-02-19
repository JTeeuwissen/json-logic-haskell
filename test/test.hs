{-# LANGUAGE OverloadedLists #-}

import Data.List
import Data.Ord
import Json (Json (JsonArray, JsonNull, JsonNumber, JsonObject))
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
    [ testCase "Simple plus" $
        H.assertEqual
          "Result is correct"
          (Right $ JsonNumber 3)
          (eval [] [("+", JsonArray [JsonNumber 1, JsonNumber 2])] JsonNull),
      testCase "Nested plus" $
        H.assertEqual
          "Result is correct"
          (Right $ JsonNumber 6)
          (eval [] [("+", JsonArray [JsonNumber 1, JsonObject [("+", JsonArray [JsonNumber 2, JsonNumber 3])]])] JsonNull)
    ]
