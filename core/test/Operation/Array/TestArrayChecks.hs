{-# LANGUAGE OverloadedLists #-}

module Operation.Array.TestArrayChecks where

import Generator.Logic
import Hedgehog as H (forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import JsonLogic.Json (Json (..))
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Test.Tasty.HUnit as U
import Utils

allUnitTests :: TestTree
allUnitTests =
  testGroup
    "All unit tests"
    [ testCase "logic{\"all\":\"[[], {\">\":[1,2]}]\"} data{}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("all", JsonArray [JsonArray [], JsonObject [(">", JsonNumber 2)]])]) JsonNull),
      testCase "logic{\"all\":\"[[1,2,3], {\">\":[{\"var\":\"\"}, 0]}]\"} data{}" $
        U.assertEqual
          "True case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("all", JsonArray [JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3], JsonObject [(">", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 0])]])]) JsonNull),
      testCase "logic{\"all\":\"[{\"var\":\"x\"}, {\">=\":[{\"var\":\"\"}, 0]}]\"} data{\"x\":[-1,2,3]\"}" $
        U.assertEqual
          "False case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("all", JsonArray [JsonObject [("var", JsonString "x")], JsonObject [(">=", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 0])]])]) (JsonObject [("x", JsonArray [JsonNumber (-1), JsonNumber 2, JsonNumber 3])]))
    ]

allGeneratorTests :: TestTree
allGeneratorTests =
  testGroup
    "all generator tests"
    [ hTestProperty "Using double array" $
        property $ do
          -- Generate random data
          ((opJson, op), (arrayJson, array)) <- forAllWith (show . \((a, _), c) -> (a, c)) $ Gen.sized sizedGenNumericArrayComparisonJson
          -- Create the rule
          let rule = JsonObject [("all", JsonArray [arrayJson, opJson])]
          -- Empty array works differently
          case array of
            [] -> Right (JsonBool False) === apply [] rule JsonNull
            _:_ -> Right (JsonBool (all op array)) === apply [] rule JsonNull
    ]

someUnitTests :: TestTree
someUnitTests =
  testGroup
    "Some unit tests"
    [ testCase "logic{\"some\":\"[[], {\">\":[1,2]}]\"} data{}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("some", JsonArray [JsonArray [], JsonObject [(">", JsonNumber 2)]])]) JsonNull),
      testCase "logic{\"some\":\"[[1,2,3], {\"<\":[{\"var\":\"\"}, 0]}]\"} data{}" $
        U.assertEqual
          "False case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("some", JsonArray [JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3], JsonObject [("<", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 0])]])]) JsonNull),
      testCase "logic{\"some\":\"[{\"var\":\"x\"}, {\">\":[{\"var\":\"\"}, 0]}]\"} data{\"x\":[-1,0,1]\"}" $
        U.assertEqual
          "True case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("some", JsonArray [JsonObject [("var", JsonString "x")], JsonObject [(">", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 0])]])]) (JsonObject [("x", JsonArray [JsonNumber (-1), JsonNumber 0, JsonNumber 1])])),
      testCase "logic{\"some\":\"[{\"var\":\"pies\"}, {\"==\":[{\"var\":\"filling\"}, \"apple\"]}]\"} data{\"pies\":[{\"filling\":\"pumpkin\",\"temp\":110},{\"filling\":\"rhubarb\",\"temp\":210},{\"filling\":\"apple\",\"temp\":310}]\"}" $
        U.assertEqual
          "Object case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("some", JsonArray [JsonObject [("var", JsonString "pies")], JsonObject [("===", JsonArray [JsonObject [("var", JsonString "filling")], JsonString "apple"])]])]) (JsonObject [("pies", JsonArray [JsonObject [("filling", JsonString "pumpkin"), ("temp", JsonNumber 110)], JsonObject [("filling", JsonString "rhubarb"), ("temp", JsonNumber 210)], JsonObject [("filling", JsonString "apple"), ("temp", JsonNumber 310)]])]))
    ]

someGeneratorTests :: TestTree
someGeneratorTests =
  testGroup
    "some generator tests"
    [ hTestProperty "Using double array" $
        property $ do
          -- Generate random data
          ((opJson, op), (arrayJson, array)) <- forAllWith (show . \((a, _), c) -> (a, c)) $ Gen.sized sizedGenNumericArrayComparisonJson
          -- Create the rule
          let rule = JsonObject [("some", JsonArray [arrayJson, opJson])]
          Right (JsonBool (any op array)) === apply [] rule JsonNull
    ]

noneUnitTests :: TestTree
noneUnitTests =
  testGroup
    "None unit tests"
    [ testCase "logic{\"none\":\"[[], {\">\":[1,2]}]\"} data{}" $
        U.assertEqual
          "Empty list case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("none", JsonArray [JsonArray [], JsonObject [(">", JsonNumber 2)]])]) JsonNull),
      testCase "logic{\"none\":\"[[-3,-2,-1], {\">\":[{\"var\":\"\"}, 0]}]\"} data{}" $
        U.assertEqual
          "True case"
          (Right $ JsonBool True)
          (apply [] (JsonObject [("none", JsonArray [JsonArray [JsonNumber (-3), JsonNumber (-2), JsonNumber (-1)], JsonObject [(">", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 0])]])]) JsonNull),
      testCase "logic{\"none\":\"[{\"var\":\"x\"}, {\"<=\":[{\"var\":\"\"}, 0]}]\"} data{\"x\":[-1,2,3]\"}" $
        U.assertEqual
          "False case"
          (Right $ JsonBool False)
          (apply [] (JsonObject [("none", JsonArray [JsonObject [("var", JsonString "x")], JsonObject [("<=", JsonArray [JsonObject [("var", JsonString "")], JsonNumber 0])]])]) (JsonObject [("x", JsonArray [JsonNumber (-1), JsonNumber 2, JsonNumber 3])]))
    ]

noneGeneratorTests :: TestTree
noneGeneratorTests =
  testGroup
    "none generator tests"
    [ hTestProperty "Using double array" $
        property $ do
          -- Generate random data
          ((opJson, op), (arrayJson, array)) <- forAllWith (show . \((a, _), c) -> (a, c)) $ Gen.sized sizedGenNumericArrayComparisonJson
          -- Create the rule
          let rule = JsonObject [("none", JsonArray [arrayJson, opJson])]
          Right (JsonBool (not (any op array))) === apply [] rule JsonNull
    ]
