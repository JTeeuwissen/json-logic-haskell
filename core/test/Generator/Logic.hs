module Generator.Logic where

import Data.Bifunctor
import Data.Fixed as F
import qualified Data.Map as M
import Generator.Generic
import Generator.Utils
import Hedgehog
import Hedgehog.Gen
import JsonLogic.Json

genArithmeticOperator :: Gen (Double -> Double -> Double, [Char])
genArithmeticOperator = element [((+), "+"), ((-), "-"), ((*), "*"), ((/), "/"), (F.mod', "%")]

genComparisonOperator :: Gen (Double -> Double -> Bool, [Char])
genComparisonOperator = element [((<), "<"), ((>), ">"), ((<=), "<="), ((>=), ">=")]

genBetweenOperator :: Gen (Double -> Double -> Bool, [Char])
genBetweenOperator = element [((<), "<"), ((<=), "<=")]

genLogicOperator :: Gen (Bool -> Bool -> Bool, [Char])
genLogicOperator = element [((&&), "and"), ((||), "or"), ((==), "==="), ((/=), "!==")]

genArrayOperator :: Gen ([Double] -> Double, [Char])
genArrayOperator = element [(minimum, "min"), (maximum, "max"), (sum, "sum")]

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
      [ createLogicObject "and" (Prelude.&&) s,
        createLogicObject "or" (Prelude.||) s,
        createLogicObject "!==" (Prelude./=) s,
        createLogicObject "===" (Prelude.==) s
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
      [ createBoolObject "and" (Prelude.&&) s,
        createBoolObject "or" (Prelude.||) s,
        createBoolObject "!==" (Prelude./=) s,
        createBoolObject "===" (Prelude.==) s,
        createBoolObject "<" (Prelude.<) s,
        createBoolObject ">" (Prelude.>) s,
        createBoolObject "<=" (Prelude.<=) s,
        createBoolObject ">=" (Prelude.>=) s
      ]

-- Creates a generator for json logic given an operator. Can contain out of logic and comparison operators
createBoolObject :: String -> (Bool -> Bool -> Bool) -> Size -> Gen (Json, Bool)
createBoolObject str op size = do
  (s1, s2) <- genUnbalancedSizes size
  (j1, x1) <- choice [sizedGenLogicJson s1, sizedGenComparisonJson s1]
  (j2, x2) <- choice [sizedGenLogicJson s2, sizedGenComparisonJson s2]
  return (JsonObject (M.fromList [(str, JsonArray [j1, j2])]), x1 `op` x2)

-- Generates a random array using the passed function for the elements
sizedGenArrayJson :: (Size -> Gen (Json, a)) -> Size -> Gen (Json, [a])
sizedGenArrayJson subGen size = do
  sizes <- genUnbalancedSizeList size
  first JsonArray . unzip <$> mapM subGen sizes

-- Generates a random array of numbers
sizedGenNumericArrayJson :: Size -> Gen (Json, [Double])
sizedGenNumericArrayJson = sizedGenArrayJson sizedGenNumericJson

-- Generates a random array of numbers and an operation that maps these to a boolean
sizedGenNumericArrayComparisonJson :: Size -> Gen ((Json, Double -> Bool), (Json, [Double]))
sizedGenNumericArrayComparisonJson size = do
  (op1, op2) <- genComparisonOperator
  (num1, num2) <- sizedGenNumericJson (size * 2)
  array <- sizedGenNumericArrayJson size
  return ((JsonObject (M.fromList [(op2, JsonArray [JsonObject (M.fromList [("var", JsonString "")]), num1])]), flip op1 num2), array)

-- Generates a random array of numbers and a reduce operation
sizedGenNumericArrayArithmeticJson :: Size -> Gen ((Json, Double -> Double -> Double), (Json, [Double]))
sizedGenNumericArrayArithmeticJson size = do
  -- The modulo operation has bad behaviour with zero
  (op1, op2) <- Hedgehog.Gen.filter ((/=) "%" . snd) genArithmeticOperator
  array <- sizedGenNumericArrayJson size
  return ((JsonObject (M.fromList [(op2, JsonArray [JsonObject (M.fromList [("var", JsonString "current")]), JsonObject (M.fromList [("var", JsonString "accumulator")])])]), flip op1), array)

-- Generates a random Json object
sizedGenJson :: Size -> Gen (Json, ())
sizedGenJson size
  | size <= 0 =
    choice
      [ (\x -> (fst x, ())) <$> sizedGenNumericJson size,
        (\x -> (fst x, ())) <$> sizedGenBoolJson size
      ]
  | otherwise = (\x -> (fst x, ())) <$> sizedGenArrayJson sizedGenJson size
