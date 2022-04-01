{-# LANGUAGE NoImplicitPrelude #-}

-- Prevent Ormolu from putting everything on a separate line.
{- ORMOLU_DISABLE -}
module JsonLogic.Operation
  ( defaultOperations, createEnv,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    dataOperations, var, missing, missingSome, preserve,
    miscOperations, trace,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    stringOperations, cat, substr
  )
where
{- ORMOLU_ENABLE -}

import Control.Monad
import qualified Data.Map as M
import JsonLogic.Json
import JsonLogic.Operation.Array
import JsonLogic.Operation.Boolean
import JsonLogic.Operation.Data
import JsonLogic.Operation.Misc
import JsonLogic.Operation.Numeric
import JsonLogic.Operation.String
import JsonLogic.Type
import Prelude (String)

-- Default operators
defaultOperations :: Monad m => M.Map String (Function m)
defaultOperations = M.unions [arrayOperations, booleanOperations, dataOperations, miscOperations, numericOperations, stringOperations]

-- Initial environment
createEnv :: Monad m => Operations m -> Json -> JsonLogicEnv m
createEnv fs = JLEnv (M.union fs defaultOperations)
