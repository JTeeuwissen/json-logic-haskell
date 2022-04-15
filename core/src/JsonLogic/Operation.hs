{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : JsonLogic.Operation
-- Description : Internal JsonLogic operations
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental

-- Prevent Ormolu from putting everything on a separate line.
{- ORMOLU_DISABLE -}
module JsonLogic.Operation
  ( defaultOperations,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    dataOperations, var, missing, missingSome, preserve,
    miscOperations, trace,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    stringOperations, cat, substr,
    evaluateDouble, evaluateNumber, evaluateInt, evaluateBool, evaluateArray, evaluateObject, evaluateString
  )
where
{- ORMOLU_ENABLE -}

import Control.Monad
import qualified Data.Map as M
import JsonLogic.Operation.Array
import JsonLogic.Operation.Boolean
import JsonLogic.Operation.Data
import JsonLogic.Operation.Misc
import JsonLogic.Operation.Numeric
import JsonLogic.Operation.Primitive
import JsonLogic.Operation.String
import JsonLogic.Type

-- Default operators
defaultOperations :: Monad m => Operations m
defaultOperations = M.unions [arrayOperations, booleanOperations, dataOperations, miscOperations, numericOperations, stringOperations]
