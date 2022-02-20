{-# LANGUAGE LambdaCase #-}

module Operations where

import Data.Map as M
import Json

-- Initial environment with only "+" defined
createEnv :: Operations -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations = M.fromList [(Operations.+)]

-- Operation type
type Operation = (String, Function)

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

evaluateMath :: (Double -> Double -> Double) -> SubEvaluator -> Json -> Either FunctionError Json
evaluateMath operator evaluator (JsonArray [x, y]) = do
  l <- evaluateNumber evaluator x
  r <- evaluateNumber evaluator y
  return $ JsonNumber $ l `operator` r
evaluateMath _ _ _ = Left $ FunctionError "Wrong number of arguments for operator" Nothing

-- Implementation for plus
(+) :: Operation
(+) = ("+", evaluateMath (Prelude.+))

(-) :: Operation
(-) = ("-", evaluateMath (Prelude.-))

(*) :: Operation
(*) = ("-", evaluateMath (Prelude.*))

(/) :: Operation
(/) = ("-", evaluateMath (Prelude./))
