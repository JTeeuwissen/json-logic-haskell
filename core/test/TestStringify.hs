module TestStringify where

import JsonLogic.Json (stringify)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Test.Tasty.HUnit as U
import Utils

stringifyUnitTests :: TestTree
stringifyUnitTests =
  testGroup
    "Show json unit tests"
    [ testCase "show null" $
        U.assertEqual
          "Result is correct"
          ""
          (stringify jNull),
      testCase "show true" $
        U.assertEqual
          "Result is correct"
          "true"
          (stringify $ jBool True),
      testCase "show false" $
        U.assertEqual
          "Result is correct"
          "false"
          (stringify $ jBool False),
      testCase "show 1" $
        U.assertEqual
          "Result is correct"
          "1.0"
          (stringify $ jNum 1.0),
      testCase "show \"string\"" $
        U.assertEqual
          "Result is correct"
          "string"
          (stringify $ jStr "string"),
      testCase "[1,2]" $
        U.assertEqual
          "Result is correct"
          "1.0,2.0"
          (stringify $ jArr [jNum 1, jNum 2]),
      testCase "{\"var\":\"x\"}" $
        U.assertEqual
          "Result is correct"
          "[object Object]"
          (stringify $ jObj [("var", jStr "x")]),
      testCase "{\"var\":\"x\",\"hi\":null}" $
        U.assertEqual
          "Result is correct"
          "[object Object]"
          (stringify $ jObj [("var", jStr "x"), ("hi", jNull)])
    ]
