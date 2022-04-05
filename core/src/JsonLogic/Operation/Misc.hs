{-# LANGUAGE OverloadedLists #-}

-- |
-- Module      : JsonLogic.Operation.Misc
-- Description : Internal JsonLogic misc operations
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Operation.Misc (miscOperations, trace) where

import Debug.Trace (traceShow)
import JsonLogic.Json
import JsonLogic.Type
import Prelude hiding (log)

miscOperations :: Monad m => Operations m
miscOperations = [trace]

trace :: Monad m => Operation m
trace = ("trace", evaluateTrace)

evaluateTrace :: Monad m => Function m Json
evaluateTrace evaluator args vars = do
  res <- evaluator args vars
  let val = case res of
        JsonArray (item : _) -> item
        oth -> oth
  traceShow val return val
