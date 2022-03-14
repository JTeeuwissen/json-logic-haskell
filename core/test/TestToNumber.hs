module TestToNumber where

import Generator.Data (genSizedNestedJsonArray)
import Generator.Generic (genGenericJsonNumber)
import Generator.Utils (increaseSizeBy)
import Hedgehog (forAll, property)
import qualified Hedgehog as H (assert)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import JsonLogic.Json (Json (JsonArray), parseFloat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Utils

toNumberUnitTests :: TestTree
toNumberUnitTests =
  testGroup
    "Number conversion unit tests"
    -- Simple unit tests that are deterministic
    [ testCase "test for null" $ assertBool "NaN == null" (isNaN $ parseFloat jNull),
      testCase "test for bool" $ assertBool "0 == false" (isNaN $ parseFloat $ jBool False),
      testCase "test for bool" $ assertBool "1 == true" (isNaN $ parseFloat $ jBool True),
      testCase "test for number" $ assertEqual "1.0 == 1.0" 1 $ parseFloat $ jNum 1.0,
      testCase "test for string" $ assertBool "0 == \"\"" (isNaN $ parseFloat $ jStr ""),
      testCase "test for string" $ assertEqual "2 == 2" 2 $ parseFloat $ jStr "2",
      testCase "test for string" $ assertEqual "1.2 == 1.2" 1.2 $ parseFloat $ jStr "1.2",
      testCase "test for string" $ assertEqual "1.2 == 1.2.3 " 1.2 $ parseFloat $ jStr "1.2.3 ",
      testCase "test for string" $ assertEqual "1 == 1abc " 1 $ parseFloat $ jStr "1abc",
      testCase "test for string" $ assertEqual "100 == 1e2 " 100 $ parseFloat $ jStr "1e2",
      testCase "test for array" $ assertBool "0 == []" (isNaN $ parseFloat $ jArr []),
      testCase "test for array" $ assertEqual "2 == [2]" 2 $ parseFloat $ jArr [jNum 2],
      testCase "test for object" $ assertBool "NaN == {}" (isNaN $ parseFloat $ jObj [])
    ]

toNumberGeneratorTests :: TestTree
toNumberGeneratorTests =
  testGroup
    "toNumber generator tests"
    [ hTestProperty "parsing works for integer strings" $
        property $ do
          (json, n) <- forAll genGenericJsonNumber
          H.assert $ parseFloat (jStr $ show json) == n,
      hTestProperty "parsing does not work for non integer strings" $
        property $ do
          -- Generate string that only contains letters
          s <- forAll $ Gen.string (Range.constant 1 10) Gen.alpha
          H.assert $ isNaN $ parseFloat $ jStr s,
      hTestProperty "parsing always returns value of first item." $
        property $ do
          -- Array with more than 1 item always results in nothing
          arr@(JsonArray as) <- forAll $ increaseSizeBy 1 $ Gen.sized genSizedNestedJsonArray
          H.assert $ case as of
            [] -> isNaN $ parseFloat arr
            (x : _) ->
              let x' = parseFloat x
                  arr' = parseFloat arr
               in (isNaN x' && isNaN arr') || (x' == arr')
    ]
