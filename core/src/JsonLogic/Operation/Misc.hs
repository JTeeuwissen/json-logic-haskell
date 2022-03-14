{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Misc (miscOperations, log, trace) where

import Debug.Trace (traceShow)
import JsonLogic.Json
import Prelude hiding (log)

miscOperations :: Operations
miscOperations = [log, trace]

log, trace :: Operation
log = ("log", undefined) -- TODO log
trace = ("trace", evaluateTrace)

evaluateTrace :: Function
evaluateTrace evaluator args vars = do
  res <- evaluator args vars
  traceShow res return res
