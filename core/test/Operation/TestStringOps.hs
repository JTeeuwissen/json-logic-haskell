{-# LANGUAGE OverloadedLists #-}

module Operation.TestStringOps where

import qualified Data.List as L
import Generator.Generic
import Hedgehog (forAll, property, (===))
import JsonLogic
import JsonLogic.Json (Json (..))
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H

inUnitTests :: TestTree
inUnitTests =
  testGroup
    "In unit tests"
    [ testCase "logic{\"in\":\"[\"\", \"test\"]\"} data{}" $
        U.assertEqual
          "Empty string case"
          (Right $ JsonBool True)
          (eval [] (JsonObject [("in", JsonArray [JsonString "", JsonString "test"])]) JsonNull),
      testCase "logic{\"in\":\"[\"Spring\", \"Springfield\"]\"} data{}" $
        U.assertEqual
          "True case"
          (Right $ JsonBool True)
          (eval [] (JsonObject [("in", JsonArray [JsonString "", JsonString "test"])]) JsonNull),
      testCase "logic{\"in\":\"[\"Test\", {\"var\":\"x\"}]\"} data{\"x\":\"testcase\"}" $
        U.assertEqual
          "False case"
          (Right $ JsonBool False)
          (eval [] (JsonObject [("in", JsonArray [JsonString "Test", JsonObject [("var", JsonString "x")]])]) (JsonObject [("x", JsonString "testcase")]))
    ]

inGeneratorTests :: TestTree
inGeneratorTests =
  testGroup
    "In generator tests"
    [ H.testProperty "in strings" $
        property $ do
          (jsonString1, string1) <- forAll genGenericJsonString
          (jsonString2, string2) <- forAll genGenericJsonString
          let rule = JsonObject [("in", JsonArray [jsonString1, jsonString2])]
              expected = JsonBool (string1 `L.isInfixOf` string2)
          Right expected === eval [] rule JsonNull
    ]
