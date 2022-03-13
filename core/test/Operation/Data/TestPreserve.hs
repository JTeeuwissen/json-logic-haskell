{-# LANGUAGE OverloadedLists #-}

module Operation.Data.TestPreserve where

import Generator.Data
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.Hedgehog as H

preserveGeneratorTests :: TestTree
preserveGeneratorTests =
  testGroup
    "preserve generator tests"
    [ H.testProperty "preserve works" $
        property $ do
          paramJson <- forAll $ Gen.sized genSizedRandomJson
          Right paramJson === eval [] (JsonObject [("preserve", paramJson)]) JsonNull
    ]
