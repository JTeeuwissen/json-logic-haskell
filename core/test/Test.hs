{-# LANGUAGE OverloadedLists #-}

import Generator.Logic (genArithmeticOperator, genArrayOperator, genBetweenOperator, genComparisonOperator, genLogicOperator)
import Hedgehog (Gen, forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json (Json (JsonArray, JsonBool, JsonNull, JsonNumber, JsonObject, JsonString))
import Operation.TestArrayChecks
import Operation.TestCat
import Operation.TestFilter
import Operation.TestIf
import Operation.TestMerge
import Operation.TestMissing
import Operation.TestMissingSome
import Operation.TestNegation
import Operation.TestPreserve
import Operation.TestReduce
import Operation.TestSubstr
import Operation.TestVar
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H
import TestStringify
import TestToNumber
import TestTruthy

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ unitTests,
      generatorTests,
      hedgehogTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ simpleUnitTests,
      ifUnitTests,
      filterUnitTests,
      varUnitTests,
      mergeUnitTests,
      mapUnitTests,
      allUnitTests,
      someUnitTests,
      noneUnitTests,
      missingUnitTests,
      missingSomeUnitTests,
      negationUnitTests,
      -- String operations
      substrUnitTests,
      catUnitTests,
      reduceUnitTests,
      -- JS casting tests
      stringifyUnitTests,
      toNumberUnitTests,
      truthyUnitTests
    ]

generatorTests :: TestTree
generatorTests =
  testGroup
    "Generator tests"
    [ truthyGeneratorTests,
      toNumberGeneratorTests,
      -- String operations
      catGeneratorTests,
      substrGeneratorTests,
      varGeneratorTests,
      missingGeneratorTests,
      missingSomeGeneratorTests,
      negationGeneratorTests,
      mergeGeneratorTests,
      preserveGeneratorTests
    ]

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
          (eval [] (JsonObject [("map", JsonArray [JsonArray [], JsonObject [("+", JsonArray [JsonNumber 1, JsonNumber 2])]])]) JsonNull),
      -- logic{"map":[[1,2], {"+", [{"var":""},2]} => [3,4]
      testCase "logic{\"map\":\"[[1,2], {\"+\":[{\"var\":\"\"}, 2]}]\"} data{}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonArray [JsonNumber 3, JsonNumber 4])
          (eval [] (JsonObject [("map", JsonArray [JsonArray [JsonNumber 1, JsonNumber 2], JsonObject [("+", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 2])]])]) JsonNull),
      -- logic{"map":[{"var":"x"}, {"+", [{"var":""},2]} data[1,2,3]=> [3,4,5]
      testCase "logic{\"map\":\"[{\"var\":\"x\"}, {\"+\":[{\"var\":\"\"}, 2]}]\"} data{\"x\":[1,2,3]\"}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonArray [JsonNumber 3, JsonNumber 4, JsonNumber 5])
          (eval [] (JsonObject [("map", JsonArray [JsonObject [("var", JsonString "x")], JsonObject [("+", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 2])]])]) (JsonObject [("x", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])]))
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
      H.testProperty "Between comparison operations" $
        property $ do
          (f, n) <- forAllWith snd genBetweenOperator
          l <- forAll genDouble
          m <- forAll genDouble
          r <- forAll genDouble
          Right (JsonBool (f l m && f m r)) === eval [] (JsonObject [(n, JsonArray [JsonNumber l, JsonNumber m, JsonNumber r])]) JsonNull,
      H.testProperty "Simple logic operations" $
        property $ do
          (f, n) <- forAllWith snd genLogicOperator
          l <- forAll Gen.bool
          r <- forAll Gen.bool
          Right (JsonBool (f l r)) === eval [] (JsonObject [(n, JsonArray [JsonBool l, JsonBool r])]) JsonNull,
      H.testProperty "Simple double array operations" $
        property $ do
          (f, n) <- forAllWith snd genArrayOperator
          arr <- forAll genDoubleArray
          Right (JsonNumber (f arr)) === eval [] (JsonObject [(n, JsonArray (map JsonNumber arr))]) JsonNull,
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

genDoubleArray :: Gen [Double]
genDoubleArray = Gen.list (Range.constant 1 50) genDouble

-- Not 0 to avoid division by zero
genDouble1 :: Gen Double
genDouble1 = Gen.double $ Range.constant 1 1000
