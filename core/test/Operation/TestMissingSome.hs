module Operation.TestMissingSome (missingSomeUnitTests, missingSomeGeneratorTests) where

import Generator.Data
import Generator.Generic (genGenericJsonNumber)
import Generator.Utils
import Hedgehog (Gen, forAll, property, (===))
import qualified Hedgehog as H (assert, failure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic
import JsonLogic.Json
import Test.Tasty
import Test.Tasty.HUnit as U
import Test.Tasty.Hedgehog as H
import Utils

missingSomeUnitTests :: TestTree
missingSomeUnitTests =
  testGroup
    "missing_some unit tests"
    [ testCase "logic {\"missing_some\":[1, []]} data {} => []" $
        U.assertEqual
          "empty case"
          (Right $ jArr [])
          (eval [] (jObj [("missing_some", jArr [jNum 0, jArr []])]) (jObj [])),
      testCase "logic {\"missing_some\":[1, [\"a\", \"b\", \"c\"]]} data {\"a\":\"apple\"} => [\"b\",\"c\"]" $
        U.assertEqual
          "first test case on site"
          (Right $ jArr [])
          (eval [] (jObj [("missing_some", jArr [jNum 1, jArr [jStr "a", jStr "b", jStr "c"]])]) (jObj [("a", jStr "apple")])),
      testCase "logic {\"missing_some\":[2, [\"a\", \"b\", \"c\"]]} data {\"a\":\"apple\"} => [\"b\",\"c\"]" $
        U.assertEqual
          "second test case on site"
          (Right $ jArr [jStr "b", jStr "c"])
          (eval [] (jObj [("missing_some", jArr [jNum 2, jArr [jStr "a", jStr "b", jStr "c"]])]) (jObj [("a", jStr "apple")]))
          -- TODO: Needs truthy and merge in order to work
          -- testCase
          --   ( "logic {\"if\" :[{\"merge\": [{\"missing\":[\"first_name\", \"last_name\"]},{\"missing_some\":[1, [\"cell_phone\", \"home_phone\"] ]}]}, \"We require first name, last name, and one phone number.\",\"OK to proceed\"]}"
          --       ++ "data {\"first_name\":\"Bruce\", \"last_name\":\"Wayne\"} => \"We require first name, last name, and one phone number.\""
          --   )
          --   $ U.assertEqual
          --     "third test case on site"
          --     (Right $ jStr "We require first name, last name, and one phone number.")
          --     (eval [] (jObj [("if", jArr [jObj [("merge", jArr [jObj [("missing", jArr [jStr "first_name", jStr "last_name"])], jObj [("missing_some", jArr [jNum 1, jArr [jStr "cell_phone", jStr "home_phone"]])]])], jStr "We require first name, last name, and one phone number.", jStr "OK to proceed"])]) (jObj [("first_name", jStr "Bruce"), ("last_name", jStr "Wayne")]))
    ]

missingSomeGeneratorTests :: TestTree
missingSomeGeneratorTests =
  testGroup
    "missing_some generator tests"
    [ H.testProperty "missing_some over emty" $
        property $ do
          -- Missing some over random integer
          (jsonNumber, _) <- forAll genGenericJsonNumber
          let missingSomeEmpty = jObj [("missing_some", jArr [jsonNumber, jArr []])]
          -- Random json data
          dataJsonArray <- forAll $ Gen.sized genSizedRandomJsonArray
          dataJsonObject <- forAll $ Gen.sized genSizedRandomJsonObject
          -- Indexing empty missing_some over random data returns empty array
          Right (jArr []) === eval [] missingSomeEmpty dataJsonArray
          Right (jArr []) === eval [] missingSomeEmpty dataJsonObject,
      H.testProperty "missing_some when all keys are present" $
        property $ do
          -- Generate flat array as data
          jsonData@(JsonArray js) <- forAll $ increaseSizeBy 1 $ Gen.sized genSizedFlatArray
          -- Generate a random list of indexes
          indexes <- forAll $ genRandomJsonIndexes (0, length js - 1)
          -- Missing a random integer when all are present will result in an empty array
          x <- forAll $ Gen.int $ Range.constant 0 30
          let rule = jObj [("missing_some", jArr [jNum $ fromIntegral x, jArr indexes])]
          Right (jArr []) === eval [] rule jsonData,
      H.testProperty "missing_some when no keys are present" $
        property $ do
          -- Generate a flat array as data
          jsonData@(JsonArray js) <- forAll $ increaseSizeBy 1 $ Gen.sized genSizedFlatArray
          -- Generates random list of indexes
          missingIndexes <- forAll $ genRandomJsonIndexes (length js, length js * 2)
          -- Construct rule
          nrMissing <- forAll $ Gen.int $ Range.constant 1 30
          let rule = jObj [("missing_some", jArr [jNum $ fromIntegral nrMissing, JsonArray missingIndexes])]
          -- Always should return entire list of missing items
          Right (jArr missingIndexes) === eval [] rule jsonData,
      H.testProperty "missing_some when some keys are present" $
        property $ do
          -- Generate a flat array as data
          jsonData@(JsonArray js) <- forAll $ increaseSizeBy 1 $ Gen.sized genSizedFlatArray
          -- On average half of the indexes are missing and half of the indexes are present
          presentIndexes <- forAll $ genRandomJsonIndexes (0, length js - 1)
          missingIndexes <- forAll $ genRandomJsonIndexes (length js, length js * 2)
          -- The amount of items required is put in a rule
          nrRequired <- forAll $ fromIntegral <$> (Gen.int $ Range.constant 0 (length js * 2) :: Gen Int)
          let rule = jObj [("missing_some", jArr [jNum nrRequired, JsonArray $ presentIndexes ++ missingIndexes])]
          -- Evaluate the rule with the data
          case eval [] rule jsonData of
            -- If there are more indexes present than the number required then it returns an empty array.
            Right (JsonArray []) -> H.assert $ fromIntegral (length presentIndexes) >= nrRequired || null missingIndexes
            -- Otherwise it returns all the missing items
            res@(Right (JsonArray _)) -> Right (jArr missingIndexes) === res
            -- Should never return any other type
            _ -> H.failure
    ]

-- | Given a range, generate a random json list with these indexes as numbers
genRandomJsonIndexes :: (Int, Int) -> Gen [Json]
genRandomJsonIndexes (lowerBound, upperBound) = map (JsonNumber . fromIntegral) <$> Gen.subsequence [lowerBound .. upperBound]
