module JsonLogic.Operation.In where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.List as L
import JsonLogic.Json (Function, Json (JsonArray, JsonBool))
import JsonLogic.Operation.Primitive (evaluateString)

evaluateIn :: Function
evaluateIn evaluator (JsonArray [sub, string]) vars = do
  sub' <- evaluateString evaluator sub vars
  string' <- evaluateString evaluator string vars
  return $ JsonBool $ L.isInfixOf sub' string'
evaluateIn _ _ _ = throwError "In received wrong agruments"
