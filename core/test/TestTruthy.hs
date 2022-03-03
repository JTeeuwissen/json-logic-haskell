module TestTruthy (truthyUnitTests, truthyGeneratorTests) where

import Generator.Data (genSizedRandomJsonArray, genSizedRandomJsonObject)
import Generator.Generic
import Hedgehog (forAll, property)
import qualified Hedgehog as H (assert)
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json
import Test.Tasty
import qualified Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H (testProperty)
import Utils

truthyUnitTests :: TestTree
truthyUnitTests =
  testGroup
    "Truthy and falsy unit tests from site"
    [ U.testCase "test for number" $ U.assertBool "falsy 0" $ falsyAssertion $ jNum 0.0,
      U.testCase "test for number" $ U.assertBool "truthy 1" $ truthyAssertion $ jNum 1.0,
      U.testCase "test for number" $ U.assertBool "truthy -1" $ truthyAssertion $ jNum $ -1.0,
      U.testCase "test for array" $ U.assertBool "falsy []" $ falsyAssertion $ jArr [],
      U.testCase "test for array" $ U.assertBool "truthy [1,2]" $ truthyAssertion $ jArr [jNum 1.0, jNum 2.0],
      U.testCase "test for string" $ U.assertBool "falsy \"\"" $ falsyAssertion $ jStr "",
      U.testCase "test for string" $ U.assertBool "truthy \"anything\"" $ truthyAssertion $ jStr "anything",
      U.testCase "test for string" $ U.assertBool "\"0\"" $ truthyAssertion $ jStr "0",
      U.testCase "test for null" $ U.assertBool "null" $ falsyAssertion jNull
    ]

truthyGeneratorTests :: TestTree
truthyGeneratorTests =
  testGroup
    "Truthy generator tests"
    [ H.testProperty "truthy for bools" $
        property $ do
          (json, b) <- forAll genGenericJsonBool
          if b
            then H.assert $ truthyAssertion json
            else H.assert $ falsyAssertion json,
      H.testProperty "truthy for numbers" $
        property $ do
          (json, n) <- forAll genGenericJsonNumber
          case n of
            0.0 -> H.assert $ falsyAssertion json
            _ -> H.assert $ truthyAssertion json,
      H.testProperty "truthy for strings" $
        property $ do
          (json, s) <- forAll genGenericJsonString
          case s of
            "" -> H.assert $ falsyAssertion json
            _ -> H.assert $ truthyAssertion json,
      H.testProperty "truthy for arrays" $
        property $ do
          jsonArr <- forAll $ Gen.sized genSizedRandomJsonArray
          case jsonArr of
            JsonArray [] -> H.assert $ falsyAssertion jsonArr
            _ -> H.assert $ truthyAssertion jsonArr,
      H.testProperty "truthy for objects" $
        property $ do
          jsonObj <- forAll $ Gen.sized genSizedRandomJsonObject
          H.assert $ truthyAssertion jsonObj
    ]

truthyAssertion :: Json -> Bool
truthyAssertion json = jsonToBool json && isTruthy json && not (isFalsy json)

falsyAssertion :: Json -> Bool
falsyAssertion json = not (jsonToBool json) && isFalsy json && not (isTruthy json)
