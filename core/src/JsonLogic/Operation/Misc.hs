{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Misc (miscOperations, log, trace) where

import JsonLogic.Json
import Prelude hiding (log)

miscOperations :: Operations
miscOperations = [log, trace]

log, trace :: Operation
log = ("log", undefined) -- TODO log
trace = ("trace", undefined) -- TODO trace
