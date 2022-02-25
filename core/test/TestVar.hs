{-# LANGUAGE OverloadedLists #-}

module TestVar where

import qualified Data.Map as M
import Generator.Logic (genArithmeticOperator, genComparisonOperator, genLogicOperator)
import Hedgehog (Gen, Size (Size), forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H

varUnitTests :: TestTree
varUnitTests =
  testGroup
    "Var unit tests"
    -- logic{"var":""} data 1 => 1
    [ testCase "logic{\"var\":\"\"} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("var", JsonString "")]) (JsonNumber 1)),
      -- logic{"var":"x"} data 1 => Null
      testCase "logic{\"var\":\"x\"} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonString "x")]) (JsonNumber 1)),
      -- logic{"var":true} data 1 => Null
      testCase "logic{\"var\":true} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonBool True)]) (JsonNumber 1)),
      -- logic{"var":"x"} data{"x":1} => 1
      testCase "logic{\"var\":\"x\"} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("var", JsonString "x")]) (JsonObject [("x", JsonNumber 1)])),
      -- logic{"var":"x"} => Null
      testCase "logic{\"var\":\"x\"} data{}" $
        U.assertEqual
          "Empty data gives Null"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonString "x")]) JsonNull),
      -- logic{"var":"x"} data{"x":[1,2,3]} => [1,2,3]
      testCase "logic{\"var\":\"x\"} data{\"x\":[1,2,3]\"}" $
        U.assertEqual
          "Substitutes arraytype correctly"
          (Right $ JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])
          (eval [] (JsonObject [("var", JsonString "x")]) (JsonObject [("x", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])])),
      -- logic{"var":"x.y"} data{"x":{"y":1}} => 1
      testCase "logic{\"var\":\"x.y\"} data{\"x\":{\"y\":1}\"}" $
        U.assertEqual
          "Access nested parameter 'x.y'"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("var", JsonString "x.y")]) (JsonObject [("x", JsonObject [("y", JsonNumber 1)])])),
      -- logic{"var":"y"} data{"x":{"y":1}} => Null
      testCase "logic{\"var\":\"y\"} data{\"x\":{\"y\":1}\"}" $
        U.assertEqual
          "Parameter not accessed on correct level"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonString "y")]) (JsonObject [("x", JsonObject [("y", JsonNumber 1)])]))
    ]
