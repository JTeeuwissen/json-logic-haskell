module JsonLogic.Operation.Filter where

import Control.Monad (filterM)
import Control.Monad.Except (MonadError (throwError))
import JsonLogic.Json (Function, Json (JsonArray))
import JsonLogic.Operation.Primitive (evaluateArray, evaluateBool)

evaluateFilter :: Function
evaluateFilter evaluator (JsonArray [xs, f]) vars = do
  array <- evaluateArray evaluator xs vars
  filtered <- filterM (evaluateBool evaluator f) array
  return $ JsonArray filtered
evaluateFilter _ _ _ = throwError "Wrong number of arguments for filter"
