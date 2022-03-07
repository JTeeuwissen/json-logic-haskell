module Operation.TestSubstr where

import Data.List (intersperse)
import Generator.Data (genSizedFlatArray)
import Generator.Generic
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JsonLogic (eval)
import JsonLogic.Json (Json (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as U (assertEqual, testCase)
import Test.Tasty.Hedgehog as H
import Utils

substrUnitTests :: TestTree
substrUnitTests =
  testGroup
    "substr unit tests"
    [ testCase "substr empty" $
        U.assertEqual
          "Empty substr should return an empty string"
          (Right $ jStr "")
          (eval [] (jObj [("substr", jArr [])]) jNull)
    ]
