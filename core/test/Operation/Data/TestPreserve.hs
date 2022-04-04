{-# LANGUAGE OverloadedLists #-}

module Operation.Data.TestPreserve where

import Generator.Data
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Utils

preserveGeneratorTests :: TestTree
preserveGeneratorTests =
  testGroup
    "preserve generator tests"
    [ hTestProperty "preserve works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right paramJson === apply [] (JsonObject [("preserve", paramJson)]) JsonNull
    ]
