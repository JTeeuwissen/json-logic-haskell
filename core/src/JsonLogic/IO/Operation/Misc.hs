{-# LANGUAGE OverloadedLists #-}

module JsonLogic.IO.Operation.Misc (miscOperations, trace, log) where

import Control.Monad.Except
import JsonLogic.IO.Mapping
import JsonLogic.IO.Type
import JsonLogic.Json
import qualified JsonLogic.Operation as O
import Prelude hiding (log)

miscOperations :: Operations
miscOperations = [trace, log]

trace, log :: Operation
trace = toOperation O.trace
log = ("log", evaluateLog)

evaluateLog :: Function Json
evaluateLog evaluator args vars = runExceptT $ do
  res <- ExceptT $ evaluator args vars
  let val = case res of
        JsonArray (item : _) -> item
        oth -> oth
  liftIO $ print val
  return val
