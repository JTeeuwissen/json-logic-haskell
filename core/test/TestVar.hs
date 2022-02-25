{-# LANGUAGE OverloadedLists #-}

module TestVar where

import qualified Data.List as L
import Generator.Data
import Generator.Generic
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H

varUnitTests :: TestTree
varUnitTests =
  testGroup
    "Var unit tests"
    -- logic{"var":""} data 1 => 1
    [ testCase "logic{\"var\":\"\"} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("var", JsonString "")]) (JsonNumber 1)),
      -- logic{"var":"x"} data 1 => Null
      testCase "logic{\"var\":\"x\"} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonString "x")]) (JsonNumber 1)),
      -- logic{"var":true} data 1 => Null
      testCase "logic{\"var\":true} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonBool True)]) (JsonNumber 1)),
      -- logic{"var":"x"} data{"x":1} => 1
      testCase "logic{\"var\":\"x\"} data{\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("var", JsonString "x")]) (JsonObject [("x", JsonNumber 1)])),
      -- logic{"var":"x"} => Null
      testCase "logic{\"var\":\"x\"} data{}" $
        U.assertEqual
          "Empty data gives Null"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonString "x")]) JsonNull),
      -- logic{"var":"x"} data{"x":[1,2,3]} => [1,2,3]
      testCase "logic{\"var\":\"x\"} data{\"x\":[1,2,3]\"}" $
        U.assertEqual
          "Substitutes arraytype correctly"
          (Right $ JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])
          (eval [] (JsonObject [("var", JsonString "x")]) (JsonObject [("x", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])])),
      -- logic{"var":"x.y"} data{"x":{"y":1}} => 1
      testCase "logic{\"var\":\"x.y\"} data{\"x\":{\"y\":1}\"}" $
        U.assertEqual
          "Access nested parameter 'x.y'"
          (Right $ JsonNumber 1)
          (eval [] (JsonObject [("var", JsonString "x.y")]) (JsonObject [("x", JsonObject [("y", JsonNumber 1)])])),
      -- logic{"var":"y"} data{"x":{"y":1}} => Null
      testCase "logic{\"var\":\"y\"} data{\"x\":{\"y\":1}\"}" $
        U.assertEqual
          "Parameter not accessed on correct level"
          (Right JsonNull)
          (eval [] (JsonObject [("var", JsonString "y")]) (JsonObject [("x", JsonObject [("y", JsonNumber 1)])]))
    ]

varGeneratorTests :: TestTree
varGeneratorTests =
  testGroup
    "Var generator tests"
    [ H.testProperty "Empty var returns entire data" $
        property $ do
          let logic = JsonObject [("var", JsonString "")]
          json <- forAll $ Gen.sized genSizedRandomJson
          Right json === eval [] logic json,
      H.testProperty "Null var returns entire data" $
        property $ do
          let logic = JsonObject [("var", JsonNull)]
          json <- forAll $ Gen.sized genSizedRandomJson
          Right json === eval [] logic json,
      H.testProperty "Bool var returns nothing" $
        property $ do
          boolJson <- forAll genGenericJsonBool
          let logic = JsonObject [("var", fst boolJson)]
          json <- forAll $ Gen.sized genSizedRandomJson
          Right JsonNull === eval [] logic json,
      H.testProperty "Number var returns index" $
        property $ do
          index <- forAll $ Gen.int $ Range.constant 0 15
          let logic = JsonObject [("var", JsonNumber $ fromIntegral index)]
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          valJson <- forAll $ Gen.sized genSizedRandomJson
          resultJson <- forAll $ return $ insertAtPath [show index] valJson randomJson
          Right valJson === eval [] logic resultJson,
      H.testProperty "String var returns item" $
        property $ do
          (indexJson, indexStr) <- forAll genGenericJsonString
          let logic = JsonObject [("var", indexJson)]
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          valJson <- forAll $ Gen.sized genSizedRandomJson
          resultJson <- forAll $ return $ insertAtPath [indexStr] valJson randomJson
          Right valJson === eval [] logic resultJson,
      H.testProperty "Nested indexing for strings returns item correctly" $
        property $ do
          recIndex <- forAll $ Gen.list (Range.constant 2 10) $ snd <$> genGenericJsonString
          let logic = JsonObject [("var", JsonString $ L.intercalate "." recIndex)]
          randomJson <- forAll $ Gen.sized genSizedRandomJsonObject
          valJson <- forAll $ Gen.sized genSizedRandomJson
          resultJson <- forAll $ return $ insertAtPath recIndex valJson randomJson
          Right valJson === eval [] logic resultJson,
      H.testProperty "Default var takes first value if it returns a value" $
        property $ do
          (stringJson, _) <- forAll genGenericJsonString
          let logic = JsonObject [("var", JsonArray [JsonNull, stringJson])]
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          Right randomJson === eval [] logic randomJson,
      H.testProperty "Defaults correctly to second value" $
        property $ do
          (stringJson, _) <- forAll genGenericJsonString
          let logic = JsonObject [("var", JsonArray [JsonBool True, stringJson])]
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          Right stringJson === eval [] logic randomJson
    ]
