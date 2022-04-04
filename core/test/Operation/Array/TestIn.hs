{-# LANGUAGE OverloadedLists #-}

module Operation.Array.TestIn where

import qualified Data.List as L
import Generator.Generic
import Hedgehog (forAll, property, (===))
import JsonLogic.Json (Json (..))
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Test.Tasty.HUnit as U
import Utils

inUnitTests :: TestTree
inUnitTests =
  testGroup
    "In unit tests"
    [ testCase "logic{\"in\":\"[\"\", \"test\"]\"} data{}" $
        U.assertEqual
          "Empty string case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("in", JsonArray [JsonString "", JsonString "test"])]) JsonNull),
      testCase "logic{\"in\":\"[\"Spring\", \"Springfield\"]\"} data{}" $
        U.assertEqual
          "True case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("in", JsonArray [JsonString "Spring", JsonString "Springfield"])]) JsonNull),
      testCase "logic{\"in\":\"[\"Test\", {\"var\":\"x\"}]\"} data{\"x\":\"testcase\"}" $
        U.assertEqual
          "False case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("in", JsonArray [JsonString "Test", JsonObject [("var", JsonString "x")]])]) (JsonObject [("x", JsonString "testcase")])),
      testCase "logic{\"in\":\"[\"\", []]\"} data{}" $
        U.assertEqual
          "Empty string case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("in", JsonArray [JsonString "", JsonArray []])]) JsonNull),
      testCase "logic{\"in\":\"[\"Ringo\", [\"John\", \"Paul\", \"George\", \"Ringo\"]]\"} data{}" $
        U.assertEqual
          "True case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("in", JsonArray [JsonString "Ringo", JsonArray [JsonString "John", JsonString "Paul", JsonString "George", JsonString "Ringo"]])]) JsonNull),
      testCase "logic{\"in\":\"[\"Ringo\", {\"var\":\"x\"}]\"} data{\"x\":[\"John\", \"Paul\", \"George\"]}" $
        U.assertEqual
          "False case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("in", JsonArray [JsonString "Ringo", JsonObject [("var", JsonString "x")]])]) (JsonObject [("x", JsonArray [JsonString "John", JsonString "Paul", JsonString "George"])]))
    ]

inGeneratorTests :: TestTree
inGeneratorTests =
  testGroup
    "In generator tests"
    [ hTestProperty "in strings" $
        property $ do
          (jsonString1, string1) <- forAll genGenericJsonString
          (jsonString2, string2) <- forAll genGenericJsonString
          let rule = JsonObject [("in", JsonArray [jsonString1, jsonString2])]
              expected = JsonBool (string1 `L.isInfixOf` string2)
          Right expected === apply [] rule JsonNull
    ]
