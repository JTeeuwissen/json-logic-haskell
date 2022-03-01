{-# LANGUAGE OverloadedLists #-}

module Operation.TestMissing where

import qualified Data.Map as M
import Generator.Data
import Generator.Generic
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H
import Utils

missingUnitTests :: TestTree
missingUnitTests =
  testGroup
    "missing unit tests"
    [ testCase "logic {\"missing\":[]} data {} => []" $
        U.assertEqual
          "empty case"
          (Right $ jArr [])
          (eval [] (jObj [("missing", jArr [])]) (jObj [])),
      testCase "logic {\"missing\":[\"a\", \"b\"]} data {\"a\":\"apple\", \"c\":\"carrot\"} => [\"b\"]" $
        U.assertEqual
          "first test case on site"
          (Right $ jArr [jStr "b"])
          (eval [] (jObj [("missing", jArr [jStr "a", jStr "b"])]) (jObj [("a", jStr "apple"), ("c", jStr "carrot")])),
      testCase "logic {\"missing\":[\"a\", \"b\"]} data {\"a\":\"apple\", \"b\":\"banana\"} => []" $
        U.assertEqual
          "second test case on site"
          (Right $ jArr [])
          (eval [] (jObj [("missing", jArr [jStr "a", jStr "b"])]) (jObj [("a", jStr "apple"), ("b", jStr "banana")])),
      testCase "logic {\"missing\":[0, 1]} data [\"apple\", \"banana\"] => []" $
        U.assertEqual
          "missing in lists"
          (Right $ jArr [])
          (eval [] (jObj [("missing", jArr [jNum 0, jNum 1])]) (jArr [jStr "apple", jStr "banana"])),
      testCase "logic {\"missing\":[0, 1]} data [\"apple\"] => [1]" $
        U.assertEqual
          "missing in lists"
          (Right $ jArr [jNum 1])
          (eval [] (jObj [("missing", jArr [jNum 0, jNum 1])]) (jArr [jStr "apple"]))
          -- TODO: Needs truthy in order to work
          -- testCase "logic {\"if\":[{\"missing\":[\"a\", \"b\"]}, \"Not enough fruit\", \"OK to proceed\"]} data {\"a\":\"apple\", \"b\":\"banana\"} => \"Ok to proceed\"" $
          --   U.assertEqual
          --     "third test case on site"
          --     (Right $ jStr "OK to proceed")
          --     (eval [] (jObj [("if", jArr [jObj [("missing", jArr [jStr "a", jStr "b"])], jStr "Not enough fruit", jStr "OK to proceed"])]) (jObj [("a", jStr "apple"), ("b", jStr "banana")]))
    ]

missingGeneratorTests :: TestTree
missingGeneratorTests =
  testGroup
    "missing generator tests"
    [ H.testProperty "missing over emty" $
        property $ do
          let missingEmpty = jObj [("missing", jArr [])]
          dataJsonArray <- forAll $ Gen.sized genSizedRandomJsonArray
          dataJsonObject <- forAll $ Gen.sized genSizedRandomJsonObject

          Right (jArr []) === eval [] missingEmpty dataJsonArray
          Right (jArr []) === eval [] missingEmpty dataJsonObject,
      H.testProperty "Using integer index on array" $
        property $ do
          jsonData <- forAll $ Gen.sized genSizedRandomJson
          x <- forAll $ Gen.int $ Range.constant 0 30
          let rule = jObj [("missing", jArr [jNum $ fromIntegral x])]
          case jsonData of
            (JsonArray js) | x < length js -> Right (jArr []) === eval [] rule jsonData
            (JsonString s) | x < length s -> Right (jArr []) === eval [] rule jsonData
            _ -> Right (jArr [jNum $ fromIntegral x]) === eval [] rule jsonData,
      H.testProperty "Using string index on array" $
        property $ do
          jsonData <- forAll $ Gen.sized genSizedRandomJsonArray
          indexString <- forAll genGenericJsonString
          let rule = jObj [("missing", jArr [fst indexString])]
          Right (jArr [fst indexString]) === eval [] rule jsonData,
      H.testProperty "Using string index on object" $
        property $ do
          jsonData <- forAll $ Gen.scale (\(Range.Size size) -> Range.Size $ size + 1) $ Gen.sized genSizedRandomJsonObject
          let (JsonObject dict) = jsonData
          indexStr <- forAll $ Gen.element $ M.keys dict
          let rule = jObj [("missing", jArr [jStr indexStr])]
          Right (jArr []) === eval [] rule jsonData,
      H.testProperty "Using integer index on object" $
        property $ do
          jsonData <- forAll $ Gen.sized genSizedRandomJsonObject
          x <- forAll $ Gen.int $ Range.constant 0 30
          let rule = jObj [("missing", jArr [jNum $ fromIntegral x])]
          Right (jArr [jNum $ fromIntegral x]) === eval [] rule jsonData
    ]
