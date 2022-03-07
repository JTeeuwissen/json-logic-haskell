module Operation.TestMerge where

import Generator.Data
import Hedgehog as H (assert, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H
import Utils

mergeUnitTests :: TestTree
mergeUnitTests =
  testGroup
    "merge unit tests"
    [ testCase "logic {\"merge\":[]} data {} => []" $
        U.assertEqual
          "empty case"
          (Right $ jArr [])
          (eval [] (jObj [("merge", jArr [])]) (jObj [])),
      testCase "logic {\"merge\":[[1,2],[3,4]} data null => [1,2,3,4]" $
        U.assertEqual
          "first test case on site"
          (Right $ jArr [jNum 1, jNum 2, jNum 3, jNum 4])
          (eval [] (jObj [("merge", jArr [jArr [jNum 1, jNum 2], jArr [jNum 3, jNum 4]])]) jNull),
      testCase "logic {\"merge\":[1,2,[3,4]} data null => [1,2,3,4]" $
        U.assertEqual
          "second test case on site"
          (Right $ jArr [jNum 1, jNum 2, jNum 3, jNum 4])
          (eval [] (jObj [("merge", jArr [jNum 1, jNum 2, jArr [jNum 3, jNum 4]])]) jNull),
      testCase "logic {\"missing\" :{ \"merge\" : [\"vin\",{\"if\": [{\"var\":\"financing\"}, [\"apr\", \"term\"], [] ]}]}} data {\"financing\":true} => [\"vin\", \"apr\", \"term\"]" $
        U.assertEqual
          "third test case on site"
          (Right $ jArr [jStr "vin", jStr "apr", jStr "term"])
          (eval [] (jObj [("missing", jObj [("merge", jArr [jStr "vin", jObj [("if", jArr [jObj [("var", jStr "financing")], jArr [jStr "apr", jStr "term"], jArr []])]])])]) (jObj [("financing", jBool True)])),
      testCase "logic {\"missing\" :{ \"merge\" : [\"vin\",{\"if\": [{\"var\":\"financing\"}, [\"apr\", \"term\"], [] ]}]}} data {\"financing\":false} => [\"vin\"]" $
        U.assertEqual
          "fourth test case on site"
          (Right $ jArr [jStr "vin"])
          (eval [] (jObj [("missing", jObj [("merge", jArr [jStr "vin", jObj [("if", jArr [jObj [("var", jStr "financing")], jArr [jStr "apr", jStr "term"], jArr []])]])])]) (jObj [("financing", jBool False)]))
    ]

mergeGeneratorTests :: TestTree
mergeGeneratorTests =
  testGroup
    "merge generator tests"
    -- Merging a flat array does not change the array at all
    [ H.testProperty "merge a flat array stays the same" $
        property $ do
          jsonData <- forAll $ Gen.sized genSizedFlatArray
          let rule = jObj [("merge", jsonData)]
          Right jsonData === eval [] rule jsonData,
      -- Merging flattens the array at one layer, but never returns a non-list
      H.testProperty "merging decreases the nesting with one" $
        property $ do
          jsonData <- forAll $ Gen.sized genSizedNestedJsonArray
          let rule = jObj [("merge", jsonData)]
          case eval [] rule jsonData of
            Right res -> do
              -- The depth should be at least 1 (should return a list)
              H.assert $ maxJsonDepth res >= 1
              -- The depth of the resulting list decreases the maximum depth by 1
              H.assert $ maxJsonDepth res == max 1 (maxJsonDepth jsonData - 1)
            -- A nested list merge never fails
            _ -> H.failure
    ]

-- | Computes the maximum nesting of a json array
maxJsonDepth :: Json -> Int
maxJsonDepth (JsonArray as) = 1 + foldl (\a b -> max a $ maxJsonDepth b) 0 as
maxJsonDepth _ = 0
