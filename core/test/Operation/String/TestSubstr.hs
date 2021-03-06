module Operation.String.TestSubstr where

import Generator.Generic
import Hedgehog as H (assert, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic.Json (Json (..))
import JsonLogic.Pure.Evaluator (apply)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as U (assertEqual, testCase)
import Utils

substrUnitTests :: TestTree
substrUnitTests =
  testGroup
    "substr unit tests"
    [ testCase "substr empty" $
        U.assertEqual
          "Empty substr should return an empty string"
          (Right $ jStr "")
          (apply [] (jObj [("substr", jArr [])]) jNull),
      testCase "first example site" $
        U.assertEqual
          "logic {\"substr\": [\"jsonlogic\", 4]} => \"logic\""
          (Right $ jStr "logic")
          (apply [] (jObj [("substr", jArr [jStr "jsonlogic", jNum 4])]) jNull),
      testCase "second example site" $
        U.assertEqual
          "logic {\"substr\": [\"jsonlogic\", -5]} => \"logic\""
          (Right $ jStr "logic")
          (apply [] (jObj [("substr", jArr [jStr "jsonlogic", jNum $ -5])]) jNull),
      testCase "third example site" $
        U.assertEqual
          "logic {\"substr\": [\"jsonlogic\", 1, 3]} => \"son\""
          (Right $ jStr "son")
          (apply [] (jObj [("substr", jArr [jStr "jsonlogic", jNum 1, jNum 3])]) jNull),
      testCase "fourth example site" $
        U.assertEqual
          "logic {\"substr\": [\"jsonlogic\", 4, -2]} => \"son\""
          (Right $ jStr "log")
          (apply [] (jObj [("substr", jArr [jStr "jsonlogic", jNum 4, jNum $ -2])]) jNull),
      testCase "two negative indexes" $
        U.assertEqual
          "logic {\"substr\": [\"jsonlogic\", -5, -2]} => \"log\""
          (Right $ jStr "log")
          (apply [] (jObj [("substr", jArr [jStr "jsonlogic", jNum $ -5, jNum $ -2])]) jNull)
    ]

substrGeneratorTests :: TestTree
substrGeneratorTests =
  testGroup
    "substr generator tests"
    -- Indexing using positive does same as drop
    [ hTestProperty "substr with positive value" $
        property $ do
          (jsonStr, str) <- forAll genGenericJsonString
          index <- forAll $ Gen.int (Range.constant 0 $ length str)
          let rule = jObj [("substr", jArr [jsonStr, jNum $ fromIntegral index])]
          Right (jStr $ drop index str) === apply [] rule jsonStr,
      -- Indexing with negative values returns end
      hTestProperty "substr with negative value" $
        property $ do
          (jsonStr, str) <- forAll genGenericNonEmptyJsonString
          index <- forAll $ Gen.int (Range.constant (-1) (-length str))
          let rule = jObj [("substr", jArr [jsonStr, jNum $ fromIntegral index])]
          case apply [] rule jsonStr of
            -- Length is the equal to the negative index
            Right (JsonString res) -> H.assert $ length res == -index
            _ -> H.failure,
      -- The evaluation returns the same result as take . drop
      hTestProperty "substr with start and final index works like take . drop" $
        property $ do
          (jsonStr, str) <- forAll genGenericJsonString
          -- Take and drop value
          index <- forAll $ Gen.int (Range.constant 0 $ length str)
          endIndex <- forAll $ Gen.int (Range.constant 0 $ length str)
          -- Assert the result is the same
          let rule = jObj [("substr", jArr [jsonStr, jNum $ fromIntegral index, jNum $ fromIntegral endIndex])]
          Right (jStr $ take endIndex $ drop index str) === apply [] rule jsonStr,
      -- The take part also works with negative indexes
      hTestProperty "substr with start and final negative index" $
        property $ do
          (jsonStr, str) <- forAll genGenericNonEmptyJsonString
          -- Positive start index, negative end index
          index <- forAll $ Gen.int (Range.constant 0 $ length str - 1)
          endIndex <- forAll $ Gen.int (Range.constant (-1) (-length str))
          -- Create rule and evaluate
          let rule = jObj [("substr", jArr [jsonStr, jNum $ fromIntegral index, jNum $ fromIntegral endIndex])]
          case apply [] rule jsonStr of
            -- Assert the length is as expected
            Right (JsonString res) -> H.assert $ length res == max 0 (length str - index + endIndex)
            _ -> H.failure
    ]
