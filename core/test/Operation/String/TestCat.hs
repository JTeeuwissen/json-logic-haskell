module Operation.String.TestCat where

import Data.List (intersperse)
import Generator.Data (genSizedFlatArray)
import Generator.Generic
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic.Json (Json (..))
import JsonLogic.Pure.Evaluator (eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as U (assertEqual, testCase)
import Utils

catUnitTests :: TestTree
catUnitTests =
  testGroup
    "cat unit tests"
    [ testCase "cat empty" $
        U.assertEqual
          "Empty cat should return an empty string"
          (Right $ jStr "")
          (eval [] (jObj [("cat", jArr [])]) jNull),
      testCase "cat empty lists" $
        U.assertEqual
          "Empty lists should not increase length of output string"
          (Right $ jStr "")
          (eval [] (jObj [("cat", jArr [jArr [], jArr [], jArr []])]) jNull),
      testCase "first cat example website" $
        U.assertEqual
          "Result should simply concat the two strings"
          (Right $ jStr "I love pie")
          (eval [] (jObj [("cat", jArr [jStr "I love", jStr " pie"])]) jNull),
      testCase "second cat example website" $
        U.assertEqual
          "Variable substitution works correctly"
          (Right $ jStr "I love apple pie")
          (eval [] (jObj [("cat", jArr [jStr "I love ", jObj [("var", jStr "filling")], jStr " pie"])]) (jObj [("filling", jStr "apple"), ("temp", jNum 110)])),
      testCase "cat of boolean" $
        U.assertEqual
          "booleans are converted to strings correctly"
          (Right $ jStr "truefalse")
          (eval [] (jObj [("cat", jArr [jBool True, jBool False])]) jNull),
      testCase "cat of nested array" $
        U.assertEqual
          "rule gets evaluated"
          (Right $ jStr "12,3,4,5false")
          (eval [] (jObj [("cat", jArr [jStr "1", jArr [jStr "2", jStr "3", jStr "4", jStr "5"], jBool False])]) jNull),
      testCase "cat an object" $
        U.assertEqual
          "Object representation"
          (Right $ jStr "[object Object]")
          (eval [] (jObj [("cat", jArr [jObj []])]) jNull)
    ]

catGeneratorTests :: TestTree
catGeneratorTests =
  testGroup
    "Cat generator tests"
    -- List of nulls results in an empty list
    [ hTestProperty "cat null objects" $
        property $ do
          jsonNulls <- forAll $ Gen.list (Range.constant 0 20) $ return JsonNull
          let rule = jObj [("cat", jArr jsonNulls)]
          Right (jStr "") === eval [] rule jNull,
      -- Flat array of strings simply concatinates them
      hTestProperty "cat strings" $
        property $ do
          jsonStrings <- forAll $ Gen.list (Range.constant 0 20) genGenericJsonString
          let rule = jObj [("cat", jArr $ map fst jsonStrings)]
              expected = jStr (concatMap snd jsonStrings)
          Right expected === eval [] rule jNull,
      -- Concatinate list of numbers
      hTestProperty "cat numbers" $
        property $ do
          jsonNumbers <- forAll $ Gen.list (Range.constant 0 20) genGenericJsonNumber
          let rule = jObj [("cat", jArr $ map fst jsonNumbers)]
              expected = jStr (concatMap (show . snd) jsonNumbers)
          Right expected === eval [] rule jNull,
      -- A nested array of nulls, we know the number of comma's to be n-1
      hTestProperty "cat nested arrays" $
        property $ do
          jsonNulls <- forAll $ Gen.list (Range.constant 0 20) $ return JsonNull
          let rule = jObj [("cat", jArr [jArr jsonNulls])]
          Right (jStr $ replicate (length jsonNulls - 1) ',') === eval [] rule jNull,
      -- Any random object string representation should not change with empty lists interleaved
      hTestProperty "cat nested arrays with inserted empty lists" $
        property $ do
          jsonArray@(JsonArray js) <- forAll $ Gen.sized genSizedFlatArray
          let rule = jObj [("cat", jsonArray)]
              intercalatedRule = jObj [("cat", jArr $ intersperse (jArr []) js)]
          eval [] intercalatedRule jNull === eval [] rule jNull
    ]
