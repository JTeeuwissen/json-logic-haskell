{-# LANGUAGE OverloadedLists #-}

import Data.List
import qualified Data.Map as M
import Data.Ord
import Generator.Logic (genArithmeticOperator, genComparisonOperator, genLogicOperator)
import Hedgehog (Gen, Size (Size), forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json (Json (JsonArray, JsonBool, JsonNull, JsonNumber, JsonObject, JsonString))
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H
import TestIf
import TestVar

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, hedgehogTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [simpleUnitTests, varUnitTests, ifUnitTests, mapUnitTests]

simpleUnitTests :: TestTree
simpleUnitTests =
  testGroup
    "Simple unit tests"
    [ testCase "Simple plus" $
        U.assertEqual
          "Result is correct"
          (Right $ JsonNumber 3)
          (eval [] (JsonObject [("+", JsonArray [JsonNumber 1, JsonNumber 2])]) JsonNull),
      testCase "Nested plus" $
        U.assertEqual
          "Result is correct"
          (Right $ JsonNumber 6)
          (eval [] (JsonObject [("+", JsonArray [JsonNumber 1, JsonObject [("+", JsonArray [JsonNumber 2, JsonNumber 3])]])]) JsonNull)
    ]

mapUnitTests :: TestTree
mapUnitTests =
  testGroup
    "Map unit tests"
    -- logic{"map":[[], {"+", [1,2]} => []
    [ testCase "logic{\"map\":\"[[], {\"+\":[1, 2]}]\"} data{}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonArray [])
          (eval [] (JsonObject $ M.singleton "map" $ JsonArray [JsonArray [], JsonObject $ M.singleton "+" $ JsonArray [JsonNumber 1, JsonNumber 2]]) JsonNull),
      -- logic{"map":[[1,2], {"+", [{"var":""},2]} => [3,4]
      testCase "logic{\"map\":\"[[1,2], {\"+\":[{\"var\":\"\"}, 2]}]\"} data{}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonArray [JsonNumber 3, JsonNumber 4])
          (eval [] (JsonObject $ M.singleton "map" $ JsonArray [JsonArray [JsonNumber 1, JsonNumber 2], JsonObject $ M.singleton "+" $ JsonArray [JsonObject $ M.singleton "var" $ JsonString "", JsonNumber 2]]) JsonNull),
      -- logic{"map":[{"var":"x"}, {"+", [{"var":""},2]} data[1,2,3]=> [3,4,5]
      testCase "logic{\"map\":\"[{\"var\":\"x\"}, {\"+\":[{\"var\":\"\"}, 2]}]\"} data{\"x\":[1,2,3]\"}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonArray [JsonNumber 3, JsonNumber 4, JsonNumber 5])
          (eval [] (JsonObject $ M.singleton "map" $ JsonArray [JsonObject $ M.singleton "var" $ JsonString "x", JsonObject $ M.singleton "+" $ JsonArray [JsonObject $ M.singleton "var" $ JsonString "", JsonNumber 2]]) (JsonObject $ M.singleton "x" $ JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3]))
    ]

hedgehogTests :: TestTree
hedgehogTests =
  testGroup
    "Hedgehog tests"
    [ H.testProperty "Simple math operations" $
        property $ do
          (f, n) <- forAllWith snd genArithmeticOperator
          l <- forAll genDouble
          r <- forAll genDouble1
          Right (JsonNumber (f l r)) === eval [] (JsonObject [(n, JsonArray [JsonNumber l, JsonNumber r])]) JsonNull,
      H.testProperty "Simple comparison operations" $
        property $ do
          (f, n) <- forAllWith snd genComparisonOperator
          l <- forAll genDouble
          r <- forAll genDouble
          Right (JsonBool (f l r)) === eval [] (JsonObject [(n, JsonArray [JsonNumber l, JsonNumber r])]) JsonNull,
      H.testProperty "Simple logic operations" $
        property $ do
          (f, n) <- forAllWith snd genLogicOperator
          l <- forAll Gen.bool
          r <- forAll Gen.bool
          Right (JsonBool (f l r)) === eval [] (JsonObject [(n, JsonArray [JsonBool l, JsonBool r])]) JsonNull,
      H.testProperty "Nested arihmetic operations" $
        property $ do
          (f1, n1) <- forAllWith snd genArithmeticOperator
          (f2, n2) <- forAllWith snd genArithmeticOperator
          (f3, n3) <- forAllWith snd genArithmeticOperator
          ll <- forAll genDouble
          lr <- forAll genDouble1
          rl <- forAll genDouble
          rr <- forAll genDouble1
          Right (JsonNumber (f1 (f2 ll lr) (f3 rl rr))) === eval [] (JsonObject [(n1, JsonArray [JsonObject [(n2, JsonArray [JsonNumber ll, JsonNumber lr])], JsonObject [(n3, JsonArray [JsonNumber rl, JsonNumber rr])]])]) JsonNull,
      H.testProperty "Nested boolean operations" $
        property $ do
          (f1, n1) <- forAllWith snd genLogicOperator
          (f2, n2) <- forAllWith snd genComparisonOperator
          (f3, n3) <- forAllWith snd genLogicOperator
          ll <- forAll genDouble
          lr <- forAll genDouble1
          rl <- forAll Gen.bool
          rr <- forAll Gen.bool
          Right (JsonBool (f1 (f2 ll lr) (f3 rl rr))) === eval [] (JsonObject [(n1, JsonArray [JsonObject [(n2, JsonArray [JsonNumber ll, JsonNumber lr])], JsonObject [(n3, JsonArray [JsonBool rl, JsonBool rr])]])]) JsonNull
    ]

genDouble :: Gen Double
genDouble = Gen.double $ Range.constant 0 1000

-- Not 0 to avoid division by zero
genDouble1 :: Gen Double
genDouble1 = Gen.double $ Range.constant 1 1000
