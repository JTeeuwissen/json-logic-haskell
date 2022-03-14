{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Numeric (numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%)) where

import Control.Monad.Except
import qualified Data.Fixed as F
import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Type
import Prelude hiding (max, min, sum, (*), (+), (-), (/), (<), (<=), (>), (>=))
import qualified Prelude hiding (max, min, sum)
import qualified Prelude as P

numericOperations :: Operations
numericOperations = [(>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%)]

-- Implementation for double -> double -> bool operators
(>), (>=), (<), (<=) :: Operation
(>) = (">", evaluateComparison (P.>))
(>=) = (">=", evaluateComparison (P.>=))
(<) = ("<", evaluateBetween (P.<))
(<=) = ("<=", evaluateBetween (P.<=))

max, min, sum :: Operation
max = ("max", evaluateDoubleArray P.maximum)
min = ("min", evaluateDoubleArray P.minimum)
sum = ("sum", evaluateDoubleArray P.sum)

(+), (-), (*), (/), (%) :: Operation
(+) = ("+", evaluateMath (P.+))
(-) = ("-", evaluateMath (P.-))
(*) = ("*", evaluateMath (P.*))
(/) = ("/", evaluateMath (P./))
(%) = ("%", evaluateMath F.mod')

evaluateComparison :: (Double -> Double -> Bool) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateComparison operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateDouble evaluator x vars
  y' <- evaluateDouble evaluator y vars
  return $ JsonBool $ x' `operator` y'
evaluateComparison _ _ _ _ = throwError "Wrong number of arguments for comparison operator"

-- Adds the between operator to check whether a number is between two other numbers
evaluateBetween :: (Double -> Double -> Bool) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateBetween operator evaluator (JsonArray [x, y, z]) vars = do
  x' <- evaluateDouble evaluator x vars
  y' <- evaluateDouble evaluator y vars
  z' <- evaluateDouble evaluator z vars
  return $ JsonBool $ (x' `operator` y') P.&& (y' `operator` z')
-- The regular two value case of the operator
evaluateBetween operator evaluator json vars = evaluateComparison operator evaluator json vars

-- Function evaluators
evaluateMath :: (Double -> Double -> Double) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateMath operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateDouble evaluator x vars
  y' <- evaluateDouble evaluator y vars
  return $ JsonNumber $ x' `operator` y'
evaluateMath _ _ _ _ = throwError "Wrong number of arguments for math operator"

-- Evaluation for max/min
evaluateDoubleArray :: ([Double] -> Double) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateDoubleArray _ _ (JsonArray []) _ = throwError "Can't evaluate array action an empty list"
evaluateDoubleArray operator evaluator (JsonArray arr) vars = do
  arr' <- mapM (\x -> evaluateDouble evaluator x vars) arr
  return $ JsonNumber $ operator arr'
evaluateDoubleArray _ _ json _ = throwError $ "Can't evaluate array action on non array, namely: " ++ show json
