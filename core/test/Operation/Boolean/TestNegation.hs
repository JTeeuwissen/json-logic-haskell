{-# LANGUAGE OverloadedLists #-}

module Operation.Boolean.TestNegation where

import Generator.Data
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.HUnit as U
import Utils

negationUnitTests :: TestTree
negationUnitTests =
  testGroup
    "negation unit tests"
    [ testCase "logic: {\"!\": [true]} data: null" $
        U.assertEqual
          "Negated true is false"
          (Right (JsonBool False))
          (apply [] (JsonObject [("!", JsonArray [JsonBool True])]) JsonNull),
      testCase "logic: {\"!\": true} data: null" $
        U.assertEqual
          "Negated true is false"
          (Right (JsonBool False))
          (apply [] (JsonObject [("!", JsonBool True)]) JsonNull),
      testCase "logic: {\"!!\": [ [] ] } data: null" $
        U.assertEqual
          "Empty array is false"
          (Right (JsonBool False))
          (apply [] (JsonObject [("!!", JsonArray [JsonArray []])]) JsonNull),
      testCase "logic: {\"!!\": [\"0\"] } data: null" $
        U.assertEqual
          "Non empty array is true"
          (Right (JsonBool True))
          (apply [] (JsonObject [("!!", JsonArray [JsonString "0"])]) JsonNull)
    ]

negationGeneratorTests :: TestTree
negationGeneratorTests =
  testGroup
    "negation generator tests"
    [ hTestProperty "negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isFalsy paramJson) === apply [] (JsonObject [("!", JsonObject [("preserve", paramJson)])]) JsonNull,
      hTestProperty "double negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isTruthy paramJson) === apply [] (JsonObject [("!!", JsonObject [("preserve", paramJson)])]) JsonNull
    ]
