module JsonLogic.Operation.If where

import Control.Monad.Except (MonadError (throwError))
import JsonLogic.Json (Function, Json (JsonArray))
import JsonLogic.Operation.Primitive

evaluateIf :: Function
evaluateIf evaluator (JsonArray [c, x, y]) vars = do
  res <- evaluateBool evaluator c vars
  if res
    then evaluator x vars
    else evaluator y vars
evaluateIf _ _ _ = throwError "Wrong number of arguments for if"
