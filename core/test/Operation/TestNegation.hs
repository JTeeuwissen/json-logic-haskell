{-# LANGUAGE OverloadedLists #-}

module Operation.TestNegation where

import Generator.Data
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.Hedgehog as H

negationGeneratorTests :: TestTree
negationGeneratorTests =
  testGroup
    "negation unit tests"
    [ H.testProperty "negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isFalsy paramJson) === eval [] (JsonObject [("!", paramJson)]) JsonNull,
      H.testProperty "double negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isTruthy paramJson) === eval [] (JsonObject [("!!", paramJson)]) JsonNull
    ]