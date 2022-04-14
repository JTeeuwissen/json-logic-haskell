{-# LANGUAGE OverloadedLists #-}

module Operation.Boolean.TestEquality where

import Generator.Logic
import Hedgehog as H (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import qualified Test.Tasty.HUnit as U
import Utils

-- See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
equalityUnitTests :: TestTree
equalityUnitTests =
  testGroup
    "Equality unit tests"
    [ testCase "Numbers" $ assertEqual (JsonNumber 1) (JsonNumber 1) True,
      testCase "Strings" $ assertEqual (JsonString "hello") (JsonString "hello") True,
      testCase "Strings and Numbers" $ assertEqual (JsonString "1") (JsonNumber 1) True,
      testCase "Numbers and Bools" $ assertEqual (JsonNumber 0) (JsonBool False) True,
      testCase "Numbers and Null" $ assertEqual (JsonNumber 0) JsonNull False,
      testCase "Numbers and not not Null" $ assertEqual (JsonNumber 0) (JsonObject [("!!", JsonNull)]) True,
      testCase "Objects" $ assertEqual (JsonObject [("preserve", JsonObject [("key", JsonString "value")])]) (JsonObject [("preserve", JsonObject [("key", JsonString "value")])]) True
    ]

equalityGeneratorTests :: TestTree
equalityGeneratorTests =
  testGroup
    "equality generator tests"
    [ hTestProperty "Object is equal to itself" $
        property $ do
          -- Generate random data
          (object, _) <- forAll $ Gen.sized sizedGenJson
          -- Create the rule
          let rule = JsonObject [("==", JsonArray [object, object])]
          Right (JsonBool True) === apply [] rule JsonNull
    ]

assertEqual :: Json -> Json -> Bool -> U.Assertion
assertEqual l r b =
  U.assertEqual
    "Result is correct"
    (Right $ JsonBool b)
    (apply [] (JsonObject [("==", JsonArray [l, r])]) JsonNull)
