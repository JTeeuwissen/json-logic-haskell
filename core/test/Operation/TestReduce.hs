{-# LANGUAGE OverloadedLists #-}

module Operation.TestReduce where

import JsonLogic
import Test.Tasty
import Test.Tasty.HUnit as U
import Utils

reduceUnitTests :: TestTree
reduceUnitTests =
  testGroup
    "reduce unit tests"
    [ testCase "logic {reduce\":[{\"var\":\"integers\"}, {\"+\":[{\"var\":\"current\"}, {\"var\":\"accumulator\"}]}, 0 ]} data {\"integers\":[1,2,3,4,5]}" $
        U.assertEqual
          "sums to 15"
          (Right $ jNum 15)
          (eval [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("+", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])], jNum 0])]) (jObj [("integers", jArr [jNum 1, jNum 2, jNum 3, jNum 4, jNum 5])])),
      testCase "logic   data  " $
        U.assertEqual
          "reduces right to left"
          (Right $ jNum 1.5)
          (eval [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("/", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])], jNum 1])]) (jObj [("integers", jArr [jNum 2, jNum 3])]))
    ]
