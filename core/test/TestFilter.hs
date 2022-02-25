{-# LANGUAGE OverloadedLists #-}

module TestFilter where

import qualified Data.Map as M
import Generator.Logic (genArithmeticOperator, genComparisonOperator, genLogicOperator)
import Hedgehog (Gen, Size (Size), forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json (Json (JsonArray, JsonBool))
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H

filterUnitTests :: TestTree
filterUnitTests =
  testGroup
    "Filter unit tests"
    [ testCase "Boolean filter values" $
        U.assertEqual
          "Return True elements"
          (Right $ JsonArray [JsonBool True, JsonBool True])
          (eval [] (JsonObject [("filter", JsonArray [JsonArray [JsonBool True, JsonBool False, JsonBool True], JsonObject [("var", JsonString "")]])]) JsonNull),
      testCase "Smaller than filter values" $
        U.assertEqual
          "Only the first item is < 2"
          (Right $ JsonArray [JsonNumber 1])
          (eval [] (JsonObject [("filter", JsonArray [JsonArray [JsonNumber 1, JsonNumber 3], JsonObject [("<", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 2])]])]) JsonNull),
      testCase "Evaluate array values" $
        U.assertEqual
          "The array value gets evaluated to True and returned."
          (Right $ JsonArray [JsonBool True])
          (eval [] (JsonObject [("filter", JsonArray [JsonArray [JsonBool False, JsonObject [("<", JsonArray [JsonNumber 1, JsonNumber 2])]], JsonObject [("var", JsonString "")]])]) JsonNull)
    ]
