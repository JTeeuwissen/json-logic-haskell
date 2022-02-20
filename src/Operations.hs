{-# LANGUAGE LambdaCase #-}

module Operations where

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
      --Logic
      (Operations.&&),
      (Operations.||),
      (Operations.!=),
      (Operations.==)
    ]

-- Operation type
type Operation = (String, Function)

-- Primitive evaluators
evaluateNumber :: SubEvaluator -> Json -> Either FunctionError Double
evaluateNumber _ (JsonNumber n) = Right n
evaluateNumber evaluator (JsonObject o) =
  either
    (Left . FunctionError "Evaluation failed" . Just)
    ( \case
        JsonNumber n -> Right n
        _ -> Left $ FunctionError "Invalid parameter type, was expecting number" Nothing
    )
    (evaluator o JsonNull)
evaluateNumber _ _ = Left $ FunctionError "Invalid parameter type, was expecting number" Nothing

evaluateBool :: SubEvaluator -> Json -> Either FunctionError Bool
evaluateBool _ (JsonBool b) = Right b
evaluateBool evaluator (JsonObject o) =
  either
    (Left . FunctionError "Evaluation failed" . Just)
    ( \case
        JsonBool b -> Right b
        _ -> Left $ FunctionError "Invalid parameter type, was expecting boolean" Nothing
    )
    (evaluator o JsonNull)
evaluateBool _ _ = Left $ FunctionError "Invalid parameter type, was expecting boolean" Nothing

-- Function evaluators
evaluateMath :: (Double -> Double -> Double) -> SubEvaluator -> Json -> Either FunctionError Json
evaluateMath operator evaluator (JsonArray [x, y]) = do
  x' <- evaluateNumber evaluator x
  y' <- evaluateNumber evaluator y
  return $ JsonNumber $ x' `operator` y'
evaluateMath _ _ _ = Left $ FunctionError "Wrong number of arguments for math operator" Nothing

evaluateComparison :: (Double -> Double -> Bool) -> SubEvaluator -> Json -> Either FunctionError Json
evaluateComparison operator evaluator (JsonArray [x, y]) = do
  x' <- evaluateNumber evaluator x
  y' <- evaluateNumber evaluator y
  return $ JsonBool $ x' `operator` y'
evaluateComparison _ _ _ = Left $ FunctionError "Wrong number of arguments for comparison operator" Nothing

evaluateLogic :: (Bool -> Bool -> Bool) -> SubEvaluator -> Json -> Either FunctionError Json
evaluateLogic operator evaluator (JsonArray [x, y]) = do
  x' <- evaluateBool evaluator x
  y' <- evaluateBool evaluator y
  return $ JsonBool $ x' `operator` y'
evaluateLogic _ _ _ = Left $ FunctionError "Wrong number of arguments for logic operator" Nothing

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