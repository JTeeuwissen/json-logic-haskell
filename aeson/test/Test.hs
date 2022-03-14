module Main where

import qualified Data.Map as M
import Generator (genJson)
import Hedgehog (forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Aeson (readJson)
import JsonLogic.Json (Json (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as U (assertEqual, testCase)
import Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parse tests" [unitTests, generatorTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests for parsing"
    [ U.testCase "test for null" $
        U.assertEqual
          "JsonNull == null"
          (Just JsonNull)
          (readJson "null"),
      U.testCase "test for bool" $
        U.assertEqual
          "JsonBool True == true"
          (Just $ JsonBool True)
          (readJson "true"),
      U.testCase "test for bool" $
        U.assertEqual
          "JsonBool False == false"
          (Just $ JsonBool False)
          (readJson "false"),
      U.testCase "test for number" $
        U.assertEqual
          "JsonNumber 3.0 == 3"
          (Just $ JsonNumber 3.0)
          (readJson "3"),
      U.testCase "test for string" $
        U.assertEqual
          "JsonString \"\" == \"\""
          (Just $ JsonString "")
          (readJson "\"\""),
      U.testCase "test for string" $
        U.assertEqual
          "JsonString \"hello world\" == \"hello world\""
          (Just $ JsonString "hello world")
          (readJson "\"hello world\""),
      U.testCase "test for array" $
        U.assertEqual
          "JsonArray [] == []"
          (Just $ JsonArray [])
          (readJson "[]"),
      U.testCase "test for array" $
        U.assertEqual
          "JsonArray [JsonNumber 1, JsonNumber 2] == [1,2]"
          (Just $ JsonArray [JsonNumber 1, JsonNumber 2])
          (readJson "[1,2]"),
      U.testCase "test for object" $
        U.assertEqual
          "JsonObject {} == {}"
          (Just $ JsonObject M.empty)
          (readJson "{}"),
      U.testCase "test for object" $
        U.assertEqual
          "JsonObject == {\"n1\":null, \"n2\":1.0}"
          (Just $ JsonObject $ M.fromList [("n1", JsonNull), ("n2", JsonNumber 1.0)])
          (readJson "{\"n1\":null, \"n2\":1.0}")
    ]

generatorTests :: TestTree
generatorTests =
  testGroup
    "Generator tests"
    [ hTestProperty "Json parsed correctly" $
        withTests 500 $
          property $ do
            (json, jsonString) <- forAll $ Gen.sized genJson
            Just json === readJson jsonString
    ]
