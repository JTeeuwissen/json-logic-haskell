module Operation.TestMerge where

import qualified Data.Map as M
import Generator.Data
import Generator.Generic
import Generator.Utils
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H
import Text.Read (readMaybe)
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
    [ H.testProperty "merge a flat array" $
        property $ do
          jsonData <- forAll $ Gen.sized genSizedFlatArray
          let rule = jObj [("merge", jsonData)]
          Right jsonData === eval [] rule jsonData
    ]
