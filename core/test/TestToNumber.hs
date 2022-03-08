module TestToNumber where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Generator.Data (genSizedNestedJsonArray)
import Generator.Generic (genGenericJsonNumber)
import Generator.Utils (increaseSizeBy)
import Hedgehog (forAll, property)
import qualified Hedgehog as H (assert)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import JsonLogic.Json (Json (JsonArray), toNumber)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog as H (testProperty)
import Utils

toNumberUnitTests :: TestTree
toNumberUnitTests =
  testGroup
    "Number conversion unit tests"
    -- Simple unit tests that are deterministic
    [ testCase "test for null" $ assertEqual "NaN == null" Nothing $ toNumber jNull,
      testCase "test for bool" $ assertEqual "0 == false" (Just 0.0) $ toNumber $ jBool False,
      testCase "test for bool" $ assertEqual "1 == true" (Just 1.0) $ toNumber $ jBool True,
      testCase "test for number" $ assertEqual "1.0 == 1.0" (Just 1.0) $ toNumber $ jNum 1.0,
      testCase "test for string" $ assertEqual "0 == \"\"" (Just 0.0) $ toNumber $ jStr "",
      testCase "test for array" $ assertEqual "0 == []" (Just 0.0) $ toNumber $ jArr [],
      testCase "test for object" $ assertEqual "NaN == {}" Nothing $ toNumber $ jObj []
    ]

toNumberGeneratorTests :: TestTree
toNumberGeneratorTests =
  testGroup
    "toNumber generator tests"
    [ H.testProperty "parsing works for integer strings" $
        property $ do
          (json, n) <- forAll genGenericJsonNumber
          H.assert $ toNumber (jStr $ show json) == Just n,
      H.testProperty "parsing does not work for non integer strings" $
        property $ do
          -- Generate string that only contains letters
          s <- forAll $ Gen.string (Range.constant 1 10) Gen.alpha
          H.assert $ isNothing (toNumber (jStr s)),
      H.testProperty "parsing always returns nothing for list with more than 1 item" $
        property $ do
          -- Array with more than 1 item always results in nothing
          arr@(JsonArray as) <- forAll $ increaseSizeBy 1 $ Gen.sized genSizedNestedJsonArray
          when (length as > 1) $ H.assert $ isNothing $ toNumber arr
    ]
