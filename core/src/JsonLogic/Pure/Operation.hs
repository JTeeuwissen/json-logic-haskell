{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : JsonLogic.Pure.Operation
-- Description : JsonLogic Pure operations
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental

{- ORMOLU_DISABLE -}
module JsonLogic.Pure.Operation
  ( defaultOperations,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    dataOperations, var, missing, missingSome, preserve,
    miscOperations, trace,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    stringOperations, cat, substr,
    evaluateDouble, evaluateInt, evaluateBool, evaluateArray, evaluateObject, evaluateString
  )
where
{- ORMOLU_ENABLE -}
import JsonLogic.Json
import qualified JsonLogic.Operation as O
import JsonLogic.Pure.Mapping
import JsonLogic.Pure.Type
import qualified Prelude as P

-- Default operators
defaultOperations :: Operations
defaultOperations = toOperations O.defaultOperations

arrayOperations, booleanOperations, dataOperations, miscOperations, numericOperations, stringOperations :: Operations
arrayOperations = toOperations O.arrayOperations
booleanOperations = toOperations O.arrayOperations
dataOperations = toOperations O.arrayOperations
miscOperations = toOperations O.arrayOperations
numericOperations = toOperations O.arrayOperations
stringOperations = toOperations O.arrayOperations

map, reduce, filter, all, none, some, merge, in' :: Operation
map = toOperation O.map
reduce = toOperation O.reduce
filter = toOperation O.filter
all = toOperation O.all
none = toOperation O.none
some = toOperation O.some
merge = toOperation O.merge
in' = toOperation O.in'

if', (==), (===), (!=), (!==), (!), (!!), and, or :: Operation
if' = toOperation O.if'
(==) = toOperation (O.==)
(===) = toOperation (O.===)
(!=) = toOperation (O.!=)
(!==) = toOperation (O.!==)
(!) = toOperation (O.!)
(!!) = toOperation (O.!!)
and = toOperation O.and
or = toOperation O.or

var, missing, missingSome, preserve :: Operation
var = toOperation O.var
missing = toOperation O.missing
missingSome = toOperation O.missingSome
preserve = toOperation O.preserve

trace :: Operation
trace = toOperation O.trace

(>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%) :: Operation
(>) = toOperation (O.>)
(>=) = toOperation (O.>=)
(<) = toOperation (O.<)
(<=) = toOperation (O.<=)
max = toOperation O.max
min = toOperation O.min
sum = toOperation O.sum
(+) = toOperation (O.+)
(-) = toOperation (O.-)
(*) = toOperation (O.*)
(/) = toOperation (O./)
(%) = toOperation (O.%)

cat, substr :: Operation
cat = toOperation O.cat
substr = toOperation O.substr

-- Primitive Evaluators

evaluateDouble :: Function P.Double
evaluateDouble = toFunction O.evaluateDouble

evaluateInt :: Function P.Int
evaluateInt = toFunction O.evaluateInt

evaluateBool :: Function P.Bool
evaluateBool = toFunction O.evaluateBool

evaluateArray :: Function [Json]
evaluateArray = toFunction O.evaluateArray

evaluateObject :: Function JsonObject
evaluateObject = toFunction O.evaluateObject

evaluateString :: Function P.String
evaluateString = toFunction O.evaluateString
