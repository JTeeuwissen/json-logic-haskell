{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Reduce where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (throwError))
import JsonLogic.Json
import JsonLogic.Operation.Primitive (evaluateArray)

evaluateReduce :: Function
evaluateReduce evaluator (JsonArray [arrayExp, reduceFunction, initalExp]) vars = do
  array <- evaluateArray evaluator arrayExp vars
  initial <- evaluator initalExp vars
  foldM (\acc cur -> evaluator reduceFunction (JsonObject [("current", cur), ("accumulator", acc)])) initial array
evaluateReduce _ _ _ = throwError "Wrong number of arguments for reduce"
