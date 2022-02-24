module JsonLogic.Operation.If where

import Control.Monad.Except (MonadError (throwError))
import JsonLogic.Json (Function, Json (JsonArray, JsonBool))

evaluateIf :: Function
evaluateIf evaluator (JsonArray [c, x, y]) vars = do
  res <- evaluator c vars
  case res of
    JsonBool True -> evaluator x vars
    JsonBool False -> evaluator y vars
    j -> throwError $ "Invalid parameter type, was expecting number. Got: " ++ show j
evaluateIf _ _ _ = throwError "Wrong number of arguments for if"