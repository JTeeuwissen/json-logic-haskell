module Generator.Logic where

import Data.Map as M
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range
import JsonLogic.Json
import Generator.Generic
import Generator.Utils

genArithmeticOperator :: Gen (Double -> Double -> Double, [Char])
genArithmeticOperator = element [((+), "+"), ((-), "-"), ((*), "*"), ((/), "/")]

genComparisonOperator :: Gen (Double -> Double -> Bool, [Char])
genComparisonOperator = element [((<), "<"), ((>), ">"), ((<=), "<="), ((>=), ">=")]

genLogicOperator :: Gen (Bool -> Bool -> Bool, [Char])
genLogicOperator = element [((&&), "and"), ((||), "or"), ((==), "=="), ((/=), "!=")]

-- Generator for a Json object that evaluates to a number
genNumericJson :: Gen (Json, Double)
genNumericJson = sized sizedGenNumericJson

sizedGenNumericJson :: Size -> Gen (Json, Double)
sizedGenNumericJson s@(Size size)
  | size <= 0 = genGenericJsonNumber
  | otherwise =
      choice
        [ createNumericObject "+" (Prelude.+) s,
          createNumericObject "-" (Prelude.-) s,
          createNumericObject "*" (Prelude.*) s,
          createNumericObject "/" (Prelude./) s
        ]

-- Creates numeric Json generator given the operator
createNumericObject :: String -> (Double -> Double -> a) -> Size -> Gen (Json, a)
createNumericObject str op size = do
  (s1, s2) <- genUnbalancedSizes size
  (j1, x1) <- sizedGenNumericJson s1
  (j2, x2) <- sizedGenNumericJson s2
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)

-- Generator for a Json object that evaluates to a boolean and only contains comparisons
genComparisonJson :: Gen (Json, Bool)
genComparisonJson = sized sizedGenComparisonJson

sizedGenComparisonJson :: Size -> Gen (Json, Bool)
sizedGenComparisonJson s@(Size size)
  | size <= 0 = genGenericJsonBool
  | otherwise =
      choice
        [ createNumericObject "<" (Prelude.<) s,
          createNumericObject ">" (Prelude.>) s,
          createNumericObject "<=" (Prelude.<=) s,
          createNumericObject ">=" (Prelude.>=) s
        ]

-- Generator for a Json object that evaluates to a boolean and only contains logic operations
genLogicJson :: Gen (Json, Bool)
genLogicJson = sized sizedGenLogicJson

sizedGenLogicJson :: Size -> Gen (Json, Bool)
sizedGenLogicJson s@(Size size)
  | size <= 0 = genGenericJsonBool
  | otherwise =
      choice
        [ createLogicObject "&&" (Prelude.&&) s,
          createLogicObject "||" (Prelude.||) s,
          createLogicObject "!=" (Prelude./=) s,
          createLogicObject "==" (Prelude.==) s
        ]

-- Creates the Logic generator given the operator
createLogicObject :: String -> (Bool -> Bool -> Bool) -> Size -> Gen (Json, Bool)
createLogicObject str op size = do
  (s1, s2) <- genUnbalancedSizes size
  (j1, x1) <- sizedGenLogicJson s1
  (j2, x2) <- sizedGenLogicJson s2
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)

-- Generates Json that evaluates to a boolean. Contains both logic and comparison operators
genBoolJson :: Gen (Json, Bool)
genBoolJson = sized sizedGenBoolJson

sizedGenBoolJson :: Size -> Gen (Json, Bool)
sizedGenBoolJson s@(Size size)
  | size <= 0 = genGenericJsonBool
  | otherwise =
      choice
        [ createBoolObject "&&" (Prelude.&&) s,
          createBoolObject "||" (Prelude.||) s,
          createBoolObject "!=" (Prelude./=) s,
          createBoolObject "==" (Prelude.==) s,
          createBoolObject "<" (Prelude.<) s,
          createBoolObject ">" (Prelude.>) s,
          createBoolObject "<=" (Prelude.<=) s,
          createBoolObject ">=" (Prelude.>=) s
        ]

-- Creates a generator for json logic given an operator. Can contain out of logic and comparison operators
createBoolObject :: String -> (Bool -> Bool -> Bool) -> Size -> Gen (Json, Bool)
createBoolObject str op s@(Size size) = do
  (s1, s2) <- genUnbalancedSizes s
  (j1, x1) <- choice [sizedGenLogicJson s1, sizedGenComparisonJson s1]
  (j2, x2) <- choice [sizedGenLogicJson s2, sizedGenComparisonJson s2]
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)
