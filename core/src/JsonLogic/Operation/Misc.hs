{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Misc (miscOperations, trace) where

import Debug.Trace (traceShow)
import JsonLogic.Type
import Prelude hiding (log)

miscOperations :: Monad m => Operations m
miscOperations = [trace]

trace :: Monad m => Operation m
trace = ("trace", evaluateTrace)

evaluateTrace :: Monad m => Function m
evaluateTrace evaluator args vars = do
  res <- evaluator args vars
  traceShow res return res
