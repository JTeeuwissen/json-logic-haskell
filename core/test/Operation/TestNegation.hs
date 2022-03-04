{-# LANGUAGE OverloadedLists #-}

module Operation.TestNegation where

import qualified Data.List as L
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

negationGeneratorTests :: TestTree
negationGeneratorTests =
  testGroup
    "negation unit tests"
    [ H.testProperty "negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ isTruthy paramJson) === eval [] (JsonObject [("!", paramJson)]) JsonNull,
      H.testProperty "double negation works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right (JsonBool $ not $ isTruthy paramJson) === eval [] (JsonObject [("!!", paramJson)]) JsonNull
    ]