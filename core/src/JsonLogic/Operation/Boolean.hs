{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Boolean (booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or) where

import Control.Monad.Except
import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Operation.Utils
import JsonLogic.Type
import Prelude hiding (all, and, any, filter, map, max, min, or, sum, (!!), (&&), (==), (||))
import qualified Prelude as P hiding (and, or)

booleanOperations :: Monad m => Operations m
booleanOperations = [if', (==), (===), (!=), (!==), (!), (!!), and, or]

if' :: Monad m => Operation m
if' = ("if", evaluateIf)

-- Implementation for bool -> bool -> bool operators
(==), (===), (!=), (!==), (!), (!!), and, or :: Monad m => Operation m
(==) = ("==", undefined) -- TODO eq
(===) = ("==", evaluateLogic (P.==))
(!=) = ("!=", undefined) -- TODO neq
(!==) = ("!=", evaluateLogic (P./=))
(!) = ("!", evaluateFalsey)
(!!) = ("!!", evaluateTruthy)
and = ("and", evaluateLogic (P.&&))
or = ("or", evaluateLogic (P.||))

evaluateIf :: Monad m => Function m Json
evaluateIf evaluator (JsonArray [c, x, y]) vars = do
  res <- evaluateBool evaluator c vars
  evaluator (if res then x else y) vars
evaluateIf _ _ _ = throw "Wrong number of arguments for if"

-- Helper functions

evaluateLogic :: Monad m => (Bool -> Bool -> Bool) -> Function m Json
evaluateLogic operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateBool evaluator x vars
  y' <- evaluateBool evaluator y vars
  return $ JsonBool $ x' `operator` y'
evaluateLogic _ _ _ _ = throw "Wrong number of arguments for logic operator"

evaluateTruthy :: Monad m => Function m Json
evaluateTruthy evaluator json vars = JsonBool <$> evaluateBool evaluator (evaluateUnaryArgument json) vars

evaluateFalsey :: Monad m => Function m Json
evaluateFalsey evaluator json vars = JsonBool . not <$> evaluateBool evaluator (evaluateUnaryArgument json) vars
