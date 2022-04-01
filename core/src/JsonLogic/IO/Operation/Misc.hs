{-# LANGUAGE OverloadedLists #-}

module JsonLogic.IO.Operation.Misc (miscOperations, trace, log) where

import Control.Monad.Except
import JsonLogic.IO.Type
import qualified JsonLogic.Operation as O
import qualified JsonLogic.Operation.Misc as M
import Prelude hiding (log)

miscOperations :: Operations
miscOperations = [M.trace, log]

trace, log :: Operation
trace = O.trace
log = ("log", evaluateLog)

evaluateLog :: Function
evaluateLog evaluator args vars = do
  res <- evaluator args vars
  liftIO $ print res --TODO proper printing
  return res
