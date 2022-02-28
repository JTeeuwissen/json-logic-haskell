{-# LANGUAGE OverloadedLists #-}

module TestIf where

import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U

ifUnitTests :: TestTree
ifUnitTests =
  testGroup
    "If unit tests"
    [ testCase "if with jsonbool" $
        U.assertEqual
          "condition is true and first value is returned"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("if", JsonArray [JsonBool True, JsonNumber 1, JsonNumber 2])]) JsonNull),
      testCase "if with rule as condition" $
        U.assertEqual
          "rule is evaluated to false and second value is returned"
          (Right $ JsonNumber 2)
          (eval [] (JsonObject [("if", JsonArray [JsonObject [(">=", JsonArray [JsonNumber 1, JsonNumber 2])], JsonNumber 1, JsonNumber 2])]) JsonNull),
      testCase "if with undefined value" $
        U.assertEqual
          "only first value is and undefined is not"
          (Right $ JsonBool True)
          (eval [] (JsonObject [("if", JsonArray [JsonObject [("<=", JsonArray [JsonNumber 1, JsonNumber 2])], JsonBool True, JsonNumber undefined])]) JsonNull),
      testCase "if with rule as value" $
        U.assertEqual
          "rule gets evaluated"
          (Right $ JsonBool True)
          (eval [] (JsonObject [("if", JsonArray [JsonBool True, JsonObject [("<=", JsonArray [JsonNumber 1, JsonNumber 2])], JsonNumber 2])]) JsonNull)
    ]
