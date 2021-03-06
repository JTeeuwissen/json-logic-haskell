{-# LANGUAGE OverloadedLists #-}

module Operation.Array.TestReduce where

import Generator.Logic
import Hedgehog as H (forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json (Json (..))
import JsonLogic.Pure.Evaluator
import JsonLogic.Pure.Type (Exception (EvalException))
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
          (apply [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("+", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])], jNum 0])]) (jObj [("integers", jArr [jNum 1, jNum 2, jNum 3, jNum 4, jNum 5])])),
      testCase "Reduce right to left" $
        U.assertEqual
          "reduces right to left"
          (Right $ jNum 1.5)
          (apply [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("/", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])], jNum 1])]) (jObj [("integers", jArr [jNum 2, jNum 3])])),
      testCase "Errors with invalid arguments" $
        U.assertEqual
          "Default value missing"
          (Left $ EvalException "Wrong number of arguments for reduce")
          (apply [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("/", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])]])]) jNull),
      testCase "Evaluate initial value" $
        U.assertEqual
          "Initial value evaluates to 1"
          (Right $ jNum 6)
          (apply [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("+", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])], jObj [("var", jStr "integer")]])]) (jObj [("integer", jNum 1), ("integers", jArr [jNum 2, jNum 3])])),
      testCase "Empty list" $
        U.assertEqual
          "Returns initial value"
          (Right $ jStr "abc")
          (apply [] (jObj [("reduce", jArr [jObj [("var", jStr "integers")], jObj [("+", jArr [jObj [("var", jStr "current")], jObj [("var", jStr "accumulator")]])], jStr "abc"])]) (jObj [("integers", jArr [])]))
    ]

reduceGeneratorTests :: TestTree
reduceGeneratorTests =
  testGroup
    "reduce generator tests"
    [ hTestProperty "Using double array" $
        property $ do
          -- Generate random data
          ((opJson, op), (arrayJson, array)) <- forAllWith (show . \((a, _), c) -> (a, c)) $ Gen.sized sizedGenNumericArrayArithmeticJson
          (nJson, n) <- forAll genNumericJson
          -- Create the rule
          let rule = JsonObject [("reduce", JsonArray [arrayJson, opJson, nJson])]
          Right (JsonNumber (foldl op n array)) === apply [] rule JsonNull
    ]
