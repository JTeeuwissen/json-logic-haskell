module Operations where

import Data.Map as M
import Json

-- Initial environment with only "+" defined
createEnv :: Functions -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations = M.fromList [("+", plus)]

-- Operation type
type Operation = (String, Function)

-- Implementation for plus
plus :: SubEvaluator -> Json -> FunctionResult
plus evaluator (JsonArray [x, y]) = do
  l <- evaluateJson x
  r <- evaluateJson y
  case (l, r) of
    (JsonNumber l', JsonNumber r') -> Right $ JsonNumber (l' + r')
    _ -> Left $ FunctionError "Cannot add non-numbers" Nothing
  where
    evaluateJson :: Json -> FunctionResult
    evaluateJson (JsonNumber n) = Right $ JsonNumber n
    evaluateJson (JsonObject o) = either (Left . FunctionError "Evaluation failed" . Just) Right (evaluator o JsonNull)
    evaluateJson _ = Left $ FunctionError "Invalid parameter type for +" Nothing
plus _ _ = Left $ FunctionError "Wrong number of arguments for +" Nothing