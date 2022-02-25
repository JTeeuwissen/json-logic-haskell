module JsonLogic.Operation.Filter where

import Control.Monad.Except (MonadError (throwError))
import Data.Maybe (catMaybes)
import JsonLogic.Json (Function, Json (JsonArray))
import JsonLogic.Operation.Primitive (evaluateArray, evaluateBool)

evaluateFilter :: Function
evaluateFilter evaluator (JsonArray [xs, f]) vars = do
  array <- evaluateArray evaluator xs vars
  filtered <- filterEither (evaluateBool evaluator f) array
  return $ JsonArray filtered
evaluateFilter _ _ _ = throwError "Wrong number of arguments for filter"

filterEither :: (x -> Either e Bool) -> [x] -> Either e [x]
filterEither f xs = do
  maybes <-
    traverse
      ( \x -> do
          x' <- f x
          return $ if x' then Just x else Nothing
      )
      xs
  return $ catMaybes maybes