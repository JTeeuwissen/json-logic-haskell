module Generators where

import Data.Map as M
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range
import Json

genArithmeticOperator :: Gen (Double -> Double -> Double, [Char])
genArithmeticOperator = element [((+), "+"), ((-), "-"), ((*), "*"), ((/), "/")]

genComparisonOperator :: Gen (Double -> Double -> Bool, [Char])
genComparisonOperator = element [((<), "<"), ((>), ">"), ((<=), "<="), ((>=), ">=")]

genLogicOperator :: Gen (Bool -> Bool -> Bool, [Char])
genLogicOperator = element [((&&), "and"), ((||), "or"), ((==), "=="), ((/=), "!=")]

-- Generator for a Json object that evaluates to a number
genNumericJson :: Gen (Json, Double)
genNumericJson = do
  d <- double $ Range.constantFrom 1 10 100
  frequency
    [ (5, return (JsonNumber d, d)),
      (1, createNumericObject "+" (Prelude.+)),
      (1, createNumericObject "-" (Prelude.-)),
      (1, createNumericObject "*" (Prelude.*)),
      (1, createNumericObject "/" (Prelude./))
    ]

-- Creates numeric Json generator given the operator
createNumericObject :: String -> (Double -> Double -> a) -> Gen (Json, a)
createNumericObject str op = do
  (j1, x1) <- genNumericJson
  (j2, x2) <- genNumericJson
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)

-- Generator for a Json object that evaluates to a boolean and only contains comparisons
genComparisonJson :: Gen (Json, Bool)
genComparisonJson = do
  b <- bool
  frequency
    [ (5, return (JsonBool b, b)),
      (1, createNumericObject "<" (Prelude.<)),
      (1, createNumericObject ">" (Prelude.>)),
      (1, createNumericObject "<=" (Prelude.<=)),
      (1, createNumericObject ">=" (Prelude.>=))
    ]

-- Generator for a Json object that evaluates to a boolean and only contains logic operations
genLogicJson :: Gen (Json, Bool)
genLogicJson = do
  b <- bool
  frequency
    [ (5, return (JsonBool b, b)),
      (1, createLogicObject "&&" (Prelude.&&)),
      (1, createLogicObject "||" (Prelude.||)),
      (1, createLogicObject "!=" (Prelude./=)),
      (1, createLogicObject "==" (Prelude.==))
    ]

-- Creates the Logic generator given the operator
createLogicObject :: String -> (Bool -> Bool -> Bool) -> Gen (Json, Bool)
createLogicObject str op = do
  (j1, x1) <- genLogicJson
  (j2, x2) <- genLogicJson
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)

-- Generates Json that evaluates to a boolean. Contains both logic and comparison operators
genBoolJson :: Gen (Json, Bool)
genBoolJson = do
  b <- bool
  frequency
    [ (10, return (JsonBool b, b)),
      (1, createBoolObject "&&" (Prelude.&&)),
      (1, createBoolObject "||" (Prelude.||)),
      (1, createBoolObject "!=" (Prelude./=)),
      (1, createBoolObject "==" (Prelude.==)),
      (1, createBoolObject "<" (Prelude.<)),
      (1, createBoolObject ">" (Prelude.>)),
      (1, createBoolObject "<=" (Prelude.<=)),
      (1, createBoolObject ">=" (Prelude.>=))
    ]

-- Creates a generator for json logic given an operator. Can contain out of logic and comparison operators
createBoolObject :: String -> (Bool -> Bool -> Bool) -> Gen (Json, Bool)
createBoolObject str op = do
  (j1, x1) <- choice [genLogicJson, genComparisonJson]
  (j2, x2) <- choice [genLogicJson, genComparisonJson]
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)
