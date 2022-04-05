{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : JsonLogic.IO.Operation
-- Description : JsonLogic IO operations
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental

{- ORMOLU_DISABLE -}
module JsonLogic.IO.Operation
  ( defaultOperations,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    dataOperations, var, missing, missingSome, preserve,
    miscOperations, trace, log,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    stringOperations, cat, substr,
    evaluateDouble, evaluateInt, evaluateBool, evaluateArray, evaluateObject, evaluateString
  )
where
{- ORMOLU_ENABLE -}
import qualified Data.Map as M
import JsonLogic.IO.Mapping
import JsonLogic.IO.Operation.Misc (miscOperations, trace, log)
import JsonLogic.IO.Type
import JsonLogic.Json
import qualified JsonLogic.Operation as O
import qualified Prelude as P

-- | A map of all the default operations.
defaultOperations :: Operations
defaultOperations = M.unions [arrayOperations, booleanOperations, dataOperations, miscOperations, numericOperations, stringOperations]

-- | Groups of operations on similar data.
arrayOperations, booleanOperations, dataOperations,  numericOperations, stringOperations :: Operations
arrayOperations = toOperations O.arrayOperations
booleanOperations = toOperations O.booleanOperations
dataOperations = toOperations O.dataOperations
numericOperations = toOperations O.numericOperations
stringOperations = toOperations O.stringOperations

-- | Array operations.
map, reduce, filter, all, none, some, merge, in' :: Operation
map = toOperation O.map
reduce = toOperation O.reduce
filter = toOperation O.filter
all = toOperation O.all
none = toOperation O.none
some = toOperation O.some
merge = toOperation O.merge
in' = toOperation O.in'

-- | Boolean operations.
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

-- | Data operations.
var, missing, missingSome, preserve :: Operation
var = toOperation O.var
missing = toOperation O.missing
missingSome = toOperation O.missingSome
preserve = toOperation O.preserve

-- | Numeric operations.
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

-- | String operations.
cat, substr :: Operation
cat = toOperation O.cat
substr = toOperation O.substr

-- Primitive Evaluators

-- | Evaluate to a double.
evaluateDouble :: Function P.Double
evaluateDouble = toFunction O.evaluateDouble

-- | Evaluate to an int.
evaluateInt :: Function P.Int
evaluateInt = toFunction O.evaluateInt

-- | Evaluate to a bool.
evaluateBool :: Function P.Bool
evaluateBool = toFunction O.evaluateBool

-- | Evaluate to an array.
evaluateArray :: Function [Json]
evaluateArray = toFunction O.evaluateArray

-- | Evaluate to an object.
evaluateObject :: Function JsonObject
evaluateObject = toFunction O.evaluateObject

-- | Evaluate to a string.
evaluateString :: Function P.String
evaluateString = toFunction O.evaluateString
