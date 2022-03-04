{-# LANGUAGE OverloadedLists #-}

module Operation.TestNegation where

import Generator.Data
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H

negationUnitTests :: TestTree
negationUnitTests =
  testGroup
    "negation unit tests"
    [ testCase "logic: {\"!\": [true]} data: null" $
        U.assertEqual
          "Negated true is false"
          (Right (JsonBool False))
          (eval [] (JsonObject [("!", JsonArray [JsonBool True])]) JsonNull),
      testCase "logic: {\"!\": true} data: null" $
        U.assertEqual
          "Negated true is false"
          (Right (JsonBool False))
          (eval [] (JsonObject [("!", JsonBool True)]) JsonNull),
      testCase "logic: {\"!!\": [ [] ] } data: null" $
        U.assertEqual
          "Empty array is false"
          (Right (JsonBool False))
          (eval [] (JsonObject [("!!", JsonArray [JsonArray []])]) JsonNull),
      testCase "logic: {\"!!\": [\"0\"] } data: null" $
        U.assertEqual
          "Non empty array is true"
          (Right (JsonBool True))
          (eval [] (JsonObject [("!!", JsonArray [JsonString "0"])]) JsonNull)
    ]

negationGeneratorTests :: TestTree
negationGeneratorTests =
  testGroup
    "negation generator tests"
    [ H.testProperty "negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isFalsy paramJson) === eval [] (JsonObject [("!", JsonObject [("preserve", paramJson)])]) JsonNull,
      H.testProperty "double negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isTruthy paramJson) === eval [] (JsonObject [("!!", JsonObject [("preserve", paramJson)])]) JsonNull
    ]
