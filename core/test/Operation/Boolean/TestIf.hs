{-# LANGUAGE OverloadedLists #-}

module Operation.Boolean.TestIf where

import Generator.Logic
import Hedgehog as H (forAll, property, (===))
import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Test.Tasty.HUnit as U
import Utils

ifUnitTests :: TestTree
ifUnitTests =
  testGroup
    "If unit tests"
    [ testCase "if with jsonbool" $
        U.assertEqual
          "condition is true and first value is returned"
          (Right $ JsonNumber 1)
          (apply [] (JsonObject [("if", JsonArray [JsonBool True, JsonNumber 1, JsonNumber 2])]) JsonNull),
      testCase "if with rule as condition" $
        U.assertEqual
          "rule is evaluated to false and second value is returned"
          (Right $ JsonNumber 2)
          (apply [] (JsonObject [("if", JsonArray [JsonObject [(">=", JsonArray [JsonNumber 1, JsonNumber 2])], JsonNumber 1, JsonNumber 2])]) JsonNull),
      testCase "if with undefined value" $
        U.assertEqual
          "only first value is and undefined is not"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("if", JsonArray [JsonObject [("<=", JsonArray [JsonNumber 1, JsonNumber 2])], JsonBool True, JsonNumber undefined])]) JsonNull),
      testCase "if with rule as value" $
        U.assertEqual
          "rule gets evaluated"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("if", JsonArray [JsonBool True, JsonObject [("<=", JsonArray [JsonNumber 1, JsonNumber 2])], JsonNumber 2])]) JsonNull)
    ]

ifGeneratorTests :: TestTree
ifGeneratorTests =
  testGroup
    "if generator tests"
    [ hTestProperty "Using double branches" $
        property $ do
          -- Generate random data
          (pJson, p) <- forAll genBoolJson
          (b1Json, b1) <- forAll genNumericJson
          (b2Json, b2) <- forAll genNumericJson
          -- Create the rule
          let rule = JsonObject [("if", JsonArray [pJson, b1Json, b2Json])]
          Right (if p then JsonNumber b1 else JsonNumber b2) === apply [] rule JsonNull
    ]
