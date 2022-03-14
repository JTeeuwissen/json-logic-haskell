module Operation.Data.TestMissing where

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
import Text.Read (readMaybe)
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
          (eval [] (jObj [("missing", jArr [jNum 0, jNum 1])]) (jArr [jStr "apple"])),
      testCase "logic {\"if\":[{\"missing\":[\"a\", \"b\"]}, \"Not enough fruit\", \"OK to proceed\"]} data {\"a\":\"apple\", \"b\":\"banana\"} => \"Ok to proceed\"" $
        U.assertEqual
          "third test case on site"
          (Right $ jStr "OK to proceed")
          (eval [] (jObj [("if", jArr [jObj [("missing", jArr [jStr "a", jStr "b"])], jStr "Not enough fruit", jStr "OK to proceed"])]) (jObj [("a", jStr "apple"), ("b", jStr "banana")]))
    ]

missingGeneratorTests :: TestTree
missingGeneratorTests =
  testGroup
    "missing generator tests"
    [ hTestProperty "missing over emty" $
        property $ do
          -- Create rule and data
          let missingEmpty = jObj [("missing", jArr [])]
          dataJsonArray <- forAll $ Gen.sized genSizedRandomJsonArray
          dataJsonObject <- forAll $ Gen.sized genSizedRandomJsonObject
          -- Empty rule over any data returns empty array
          Right (jArr []) === eval [] missingEmpty dataJsonArray
          Right (jArr []) === eval [] missingEmpty (JsonObject dataJsonObject),
      hTestProperty "Using integer index on array" $
        property $ do
          -- Generate random data
          jsonData <- forAll $ Gen.sized genSizedRandomJson
          -- Create the rule
          x <- forAll $ Gen.int $ Range.constant 0 30
          let rule = jObj [("missing", jArr [jNum $ fromIntegral x])]
          -- Only returns empty array if the index is missing
          case jsonData of
            (JsonArray js) | x < length js -> Right (jArr []) === eval [] rule jsonData
            (JsonString s) | x < length s -> Right (jArr []) === eval [] rule jsonData
            -- If the index is a number and that index is in the dict it is indexed
            (JsonObject o) | M.member (show x) o -> Right (jArr []) === eval [] rule jsonData
            _ -> Right (jArr [jNum $ fromIntegral x]) === eval [] rule jsonData,
      hTestProperty "Using string index on array" $
        property $ do
          -- Generate random array
          jsonData@(JsonArray js) <- forAll $ Gen.sized genSizedRandomJsonArray
          (indexJson, indexStr) <- forAll genGenericNonEmptyJsonString
          -- Item should be in result since it cannot be present
          let rule = jObj [("missing", jArr [indexJson])]
          case readMaybe indexStr :: Maybe Int of
            -- The string is by chance a number, if it is within the range the index is succesful
            Just i | i >= 0 && i < length js -> Right (jArr []) === eval [] rule jsonData
            -- Index is not present since it is a string or outside the range
            _ -> Right (jArr [indexJson]) === eval [] rule jsonData,
      hTestProperty "Using string index on object" $
        property $ do
          -- Generate data
          jsonData <- forAll $ increaseSizeBy 1 $ Gen.sized genSizedRandomJsonObject
          -- Choose a random item from the object
          indexStr <- forAll $ Gen.element $ M.keys jsonData
          let rule = jObj [("missing", jArr [jStr indexStr])]
          -- The item should not be missing
          Right (jArr []) === eval [] rule (JsonObject jsonData),
      hTestProperty "Using integer index on object" $
        property $ do
          -- Random data
          jsonData <- forAll $ Gen.sized genSizedRandomJsonObject
          -- Generate random index
          x <- forAll $ Gen.int $ Range.constant 0 30
          let rule = jObj [("missing", jArr [jNum $ fromIntegral x])]
          -- Item should be missing
          if M.member (show x) jsonData
            then Right (jArr []) === eval [] rule (JsonObject jsonData)
            else Right (jArr [jNum $ fromIntegral x]) === eval [] rule (JsonObject jsonData)
    ]
