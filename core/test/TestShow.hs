{-# LANGUAGE OverloadedLists #-}

module TestShow where

import JsonLogic.Json (Json (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Test.Tasty.HUnit as U

showJsonUnitTests :: TestTree
showJsonUnitTests =
  testGroup
    "Show json unit tests"
    [ testCase "show null" $
        U.assertEqual
          "Result is correct"
          ""
          (show JsonNull),
      testCase "show true" $
        U.assertEqual
          "Result is correct"
          "true"
          (show $ JsonBool True),
      testCase "show false" $
        U.assertEqual
          "Result is correct"
          "false"
          (show $ JsonBool False),
      testCase "show 1" $
        U.assertEqual
          "Result is correct"
          "1.0"
          (show $ JsonNumber 1.0),
      testCase "show \"string\"" $
        U.assertEqual
          "Result is correct"
          "string"
          (show $ JsonString "string"),
      testCase "[1,2]" $
        U.assertEqual
          "Result is correct"
          "[1.0,2.0]"
          (show $ JsonArray [JsonNumber 1, JsonNumber 2]),
      testCase "{\"var\":\"x\"}" $
        U.assertEqual
          "Result is correct"
          "[object Object]"
          (show $ JsonObject [("var", JsonString "x")]),
      testCase "{\"var\":\"x\",\"hi\":null}" $
        U.assertEqual
          "Result is correct"
          "[object Object]"
          (show $ JsonObject [("var", JsonString "x"), ("hi", JsonNull)])
    ]
