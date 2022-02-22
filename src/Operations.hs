module Operations where

import Control.Monad.Except (MonadError (throwError))
import Data.Map as M
import Json

-- Initial environment with only "+" defined
createEnv :: Operations -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations =
  M.fromList
    [ -- Arithmetic
      (Operations.+),
      (Operations.-),
      (Operations.*),
      (Operations./),
      -- Comparison
      (Operations.<),
      (Operations.>),
      (Operations.<=),
      (Operations.>=),
      -- Logic
      (Operations.&&),
      (Operations.||),
      (Operations.!=),
      (Operations.==)
    ]

-- Operation type
type Operation = (String, Function)

-- Primitive evaluators
evaluateNumber :: SubEvaluator -> Json -> Either String Double
evaluateNumber evaluator param = do
  res <- evaluator param JsonNull
  case res of
    JsonNumber n -> return n
    _ -> throwError "Invalid parameter type, was expecting number"

evaluateBool :: SubEvaluator -> Json -> Either String Bool
evaluateBool evaluator param = do
  res <- evaluator param JsonNull
  case res of
    JsonBool b -> return b
    _ -> throwError "Invalid parameter type, was expecting boolean"

-- Function evaluators
evaluateMath :: (Double -> Double -> Double) -> SubEvaluator -> Json -> Either String Json
evaluateMath operator evaluator (JsonArray [x, y]) = do
  x' <- evaluateNumber evaluator x
  y' <- evaluateNumber evaluator y
  return $ JsonNumber $ x' `operator` y'
evaluateMath _ _ _ = throwError "Wrong number of arguments for math operator"

evaluateComparison :: (Double -> Double -> Bool) -> SubEvaluator -> Json -> Either String Json
evaluateComparison operator evaluator (JsonArray [x, y]) = do
  x' <- evaluateNumber evaluator x
  y' <- evaluateNumber evaluator y
  return $ JsonBool $ x' `operator` y'
evaluateComparison _ _ _ = throwError "Wrong number of arguments for comparison operator"

evaluateLogic :: (Bool -> Bool -> Bool) -> SubEvaluator -> Json -> Either String Json
evaluateLogic operator evaluator (JsonArray [x, y]) = do
  x' <- evaluateBool evaluator x
  y' <- evaluateBool evaluator y
  return $ JsonBool $ x' `operator` y'
evaluateLogic _ _ _ = throwError "Wrong number of arguments for logic operator"

-- Implementation for arithmetic operators

(+) :: Operation
(+) = ("+", evaluateMath (Prelude.+))

(-) :: Operation
(-) = ("-", evaluateMath (Prelude.-))

(*) :: Operation
(*) = ("*", evaluateMath (Prelude.*))

(/) :: Operation
(/) = ("/", evaluateMath (Prelude./))

-- Implementation for bool -> bool -> bool operators
(&&) :: Operation
(&&) = ("and", evaluateLogic (Prelude.&&))

(||) :: Operation
(||) = ("or", evaluateLogic (Prelude.||))

(==) :: Operation
(==) = ("==", evaluateLogic (Prelude.==)) -- TODO proper equality implementation.

(!=) :: Operation
(!=) = ("!=", evaluateLogic (Prelude./=))

-- Implementation for double -> double -> bool operators
(<) :: Operation
(<) = ("<", evaluateComparison (Prelude.<))

(>) :: Operation
(>) = (">", evaluateComparison (Prelude.>))

(<=) :: Operation
(<=) = ("<=", evaluateComparison (Prelude.<=))

(>=) :: Operation
(>=) = (">=", evaluateComparison (Prelude.>=))
