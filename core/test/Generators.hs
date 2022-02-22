module Generators where

import Hedgehog (Gen)
import Hedgehog.Gen (element, enum)

genArithmeticOperator :: Gen (Double -> Double -> Double, [Char])
genArithmeticOperator = element [((+), "+"), ((-), "-"), ((*), "*"), ((/), "/")]

genComparisonOperator :: Gen (Double -> Double -> Bool, [Char])
genComparisonOperator = element [((<), "<"), ((>), ">"), ((<=), "<="), ((>=), ">=")]

genLogicOperator :: Gen (Bool -> Bool -> Bool, [Char])
genLogicOperator = element [((&&), "and"), ((||), "or"), ((==), "=="), ((/=), "!=")]
