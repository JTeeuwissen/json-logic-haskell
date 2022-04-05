{-# LANGUAGE OverloadedLists #-}

module Operation.Data.TestVar where

import qualified Data.List as L
import Generator.Data
import Generator.Generic
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import Test.Tasty
import Test.Tasty.HUnit as U
import Utils

varUnitTests :: TestTree
varUnitTests =
  testGroup
    "Var unit tests"
    -- logic {"var":""} data 1 => 1
    [ testCase "logic {\"var\":\"\"} data {\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right $ JsonNumber 1)
          (apply [] (JsonObject [("var", JsonString "")]) (JsonNumber 1)),
      -- logic {"var":"x"} data 1 => Null
      testCase "logic {\"var\":\"x\"} data {\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonString "x")]) (JsonNumber 1)),
      -- logic {"var":true} data 1 => Null
      testCase "logic {\"var\":true} data {\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonBool True)]) (JsonNumber 1)),
      -- logic {"var":"x"} data{"x":1} => 1
      testCase "logic {\"var\":\"x\"} data {\"x\":1}" $
        U.assertEqual
          "Simple var case is correct"
          (Right $ JsonNumber 1)
          (apply [] (JsonObject [("var", JsonString "x")]) (JsonObject [("x", JsonNumber 1)])),
      -- logic {"var":"x"} => Null
      testCase "logic {\"var\":\"x\"} data {}" $
        U.assertEqual
          "Empty data gives Null"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonString "x")]) JsonNull),
      -- logic {"var":"x"} data {"x":[1,2,3]} => [1,2,3]
      testCase "logic {\"var\":\"x\"} data {\"x\":[1,2,3]\"}" $
        U.assertEqual
          "Substitutes arraytype correctly"
          (Right $ JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])
          (apply [] (JsonObject [("var", JsonString "x")]) (JsonObject [("x", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])])),
      -- logic {"var":"x.y"} data {"x":{"y":1}} => 1
      testCase "logic {\"var\":\"x.y\"} data {\"x\":{\"y\":1}\"}" $
        U.assertEqual
          "Access nested parameter 'x.y'"
          (Right $ JsonNumber 1)
          (apply [] (JsonObject [("var", JsonString "x.y")]) (JsonObject [("x", JsonObject [("y", JsonNumber 1)])])),
      -- logic {"var":"y"} data {"x":{"y":1}} => Null
      testCase "logic {\"var\":\"y\"} data {\"x\":{\"y\":1}\"}" $
        U.assertEqual
          "Parameter not accessed on correct level"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonString "y")]) $ JsonObject [("x", JsonObject [("y", JsonNumber 1)])]),
      -- logic {"var":{}} data 1 => Null
      testCase "logic {\"var\":[]} data 1 " $
        U.assertEqual
          "Indexing with an object returns null"
          (Right $ JsonNumber 1)
          (apply [] (JsonObject [("var", JsonArray [])]) $ JsonNumber 1),
      -- logic {"var":{}} data 1 => Null
      testCase "logic {\"var\":{}} data 1 " $
        U.assertEqual
          "Indexing with an object returns null"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonObject [])]) $ JsonNumber 1),
      -- logic {"var":[["y"]]} data {"y":{"y":1}} => Null
      testCase "logic {\"var\":[[\"y\"]]} data {\"y\":{\"y\":1}\"}" $
        U.assertEqual
          "Indexing with nested array gives back null"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonObject [])]) $ JsonObject [("y", JsonObject [("y", JsonNumber 1)])]),
      -- logic {"var":["", true]} data null => Null
      testCase "logic {\"var\":[\"\", true]} data null" $
        U.assertEqual
          "null value does not always return backup"
          (Right JsonNull)
          (apply [] (JsonObject [("var", JsonArray [JsonString "", JsonBool True])]) JsonNull)
    ]

varGeneratorTests :: TestTree
varGeneratorTests =
  testGroup
    "Var generator tests"
    [ hTestProperty "null, empty string, and empty array return entire data" $
        property $ do
          -- Logic that all returns the entire data
          let nullVar = JsonObject [("var", JsonNull)]
              emptyStringVar = JsonObject [("var", JsonString "")]
              emptyArrayVar = JsonObject [("var", JsonArray [])]
          dataJson <- forAll $ Gen.sized genSizedRandomJson
          -- All of them return the entire data
          Right dataJson === apply [] nullVar dataJson
          Right dataJson === apply [] emptyStringVar dataJson
          Right dataJson === apply [] emptyArrayVar dataJson,
      hTestProperty "bool, nested array, and empty object var return nothing" $
        property $ do
          -- Logic that all returns null
          boolJson <- forAll genGenericJsonBool
          arrJson <- forAll $ Gen.sized genSizedNestedJsonArray
          let boolVar = JsonObject [("var", fst boolJson)]
              arrVar = JsonObject [("var", JsonArray [arrJson])]
              objVar = JsonObject [("var", JsonObject [])]
          dataJson <- forAll $ Gen.sized genSizedRandomJson
          -- All of them return null
          Right JsonNull === apply [] boolVar dataJson
          Right JsonNull === apply [] arrVar dataJson
          Right JsonNull === apply [] objVar dataJson,
      hTestProperty "Number var returns index" $
        property $ do
          -- Insert Json data at index
          index <- forAll $ Gen.int $ Range.constant 0 15
          let logic = JsonObject [("var", JsonNumber $ fromIntegral index)]
          -- Generate random Json and random data and inject it at the index
          dataJson <- forAll $ Gen.sized genSizedRandomJson
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          resultJson <- forAll $ return $ insertAtPath [show index] dataJson randomJson
          -- Check that the index returns the data
          Right dataJson === apply [] logic resultJson,
      hTestProperty "String var returns item" $
        property $ do
          -- The member to index
          (indexJson, indexStr) <- forAll genGenericNonEmptyJsonString
          let logic = JsonObject [("var", indexJson)]
          -- Generate random Json and random data and inject it at the string member
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          dataJson <- forAll $ Gen.sized genSizedRandomJson
          resultJson <- forAll $ return $ insertAtPath [indexStr] dataJson randomJson
          -- Check that the data is found at the index
          Right dataJson === apply [] logic resultJson,
      hTestProperty "Nested indexing for strings returns item correctly" $
        property $ do
          -- Generate a list of strings denoting a path
          -- f.e ["aa", "bb"] is path "aa.bb" in Json
          recIndex <- forAll $ Gen.list (Range.constant 2 10) $ snd <$> genGenericNonEmptyJsonString
          let logic = JsonObject [("var", JsonString $ L.intercalate "." recIndex)]
          -- Generate random Json and random data and inject it at path
          randomJson <- forAll $ Gen.sized genSizedRandomJsonObject
          dataJson <- forAll $ Gen.sized genSizedRandomJson
          resultJson <- forAll $ return $ insertAtPath recIndex dataJson (JsonObject randomJson)
          -- Verify the data is found at the path in the Json
          Right dataJson === apply [] logic resultJson,
      hTestProperty "Default var takes first value if it returns a value" $
        property $ do
          -- Use var null to always convert to a valid item
          (stringJson, _) <- forAll genGenericNonEmptyJsonString
          let logic = JsonObject [("var", JsonArray [JsonNull, stringJson])]
          dataJson <- forAll $ Gen.sized genSizedRandomJsonArray
          -- Verify we get the entire data as result and not the default
          Right dataJson === apply [] logic dataJson,
      hTestProperty "Defaults correctly to second value" $
        property $ do
          -- Json bool so it always defaults to the stringJson
          (stringJson, _) <- forAll genGenericNonEmptyJsonString
          let logic = JsonObject [("var", JsonArray [JsonBool True, stringJson])]
          randomJson <- forAll $ Gen.sized genSizedRandomJsonArray
          -- Verify the default value as the result
          Right stringJson === apply [] logic randomJson
    ]
