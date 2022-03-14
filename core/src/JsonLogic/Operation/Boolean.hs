{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Boolean (booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or) where

import Control.Monad.Except
import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Operation.Utils
import JsonLogic.Type
import Prelude hiding (all, and, any, filter, map, max, min, or, sum, (!!), (&&), (==), (||))
import qualified Prelude as P hiding (and, or)

booleanOperations :: Operations
booleanOperations = [if', (==), (===), (!=), (!==), (!), (!!), and, or]

if' :: Operation
if' = ("if", evaluateIf)

-- Implementation for bool -> bool -> bool operators
(==), (===), (!=), (!==), (!), (!!), and, or :: Operation
(==) = ("==", undefined) -- TODO eq
(===) = ("==", evaluateLogic (P.==))
(!=) = ("!=", undefined) -- TODO neq
(!==) = ("!=", evaluateLogic (P./=))
(!) = ("!", evaluateFalsey)
(!!) = ("!!", evaluateTruthy)
and = ("and", evaluateLogic (P.&&))
or = ("or", evaluateLogic (P.||))

evaluateIf :: Function
evaluateIf evaluator (JsonArray [c, x, y]) vars = do
  res <- evaluateBool evaluator c vars
  evaluator (if res then x else y) vars
evaluateIf _ _ _ = throwError "Wrong number of arguments for if"

-- Helper functions

evaluateLogic :: (Bool -> Bool -> Bool) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateLogic operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateBool evaluator x vars
  y' <- evaluateBool evaluator y vars
  return $ JsonBool $ x' `operator` y'
evaluateLogic _ _ _ _ = throwError "Wrong number of arguments for logic operator"

evaluateTruthy :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateTruthy evaluator json vars = JsonBool <$> evaluateBool evaluator (evaluateUnaryArgument json) vars

evaluateFalsey :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateFalsey evaluator json vars = JsonBool . not <$> evaluateBool evaluator (evaluateUnaryArgument json) vars
