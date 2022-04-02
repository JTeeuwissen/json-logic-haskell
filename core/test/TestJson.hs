module TestJson where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Test.Tasty.HUnit as U
import Utils

parseUnitTests :: TestTree
parseUnitTests =
  testGroup
    "Read json unit tests"
    [ testCase "read null" $
        U.assertEqual
          "Result is correct"
          jNull
          (read "null"),
      testCase "read true" $
        U.assertEqual
          "Result is correct"
          (jBool True)
          (read "true"),
      testCase "read false" $
        U.assertEqual
          "Result is correct"
          (jBool False)
          (read "false"),
      testCase "read 1" $
        U.assertEqual
          "Result is correct"
          (jNum 1.0)
          (read "1.0"),
      testCase "read 1.2e-3" $
        U.assertEqual
          "Result is correct"
          (jNum 0.0012)
          (read "1.2e-3"),
      testCase "read \"string\"" $
        U.assertEqual
          "Result is correct"
          (jStr "string")
          (read "\"string\""),
      testCase "[1,2]" $
        U.assertEqual
          "Result is correct"
          (jArr [jNum 1, jNum 2])
          (read "[1.0,2.0]"),
      testCase "{\"var\":\"x\"}" $
        U.assertEqual
          "Result is correct"
          (jObj [("var", jStr "x")])
          (read "{\"var\":\"x\"}"),
      testCase "{\"var\":\"x\",\"hi\":null}" $
        U.assertEqual
          "Result is correct"
          (jObj [("var", jStr "x"), ("hi", jNull)])
          (read "{\"var\":\"x\",\"hi\":null}")
    ]
