-- Prevent Ormolu from putting everything on a separate line.
{- ORMOLU_DISABLE -}
module JsonLogic.IO.Operation
  ( createEnvIO, defaultOperationsIO,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    dataOperations, var, missing, missingSome, preserve,
    miscOperations, log, trace,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    stringOperations, cat, substr
  )
where
{- ORMOLU_ENABLE -}

import qualified Data.Map as M
import JsonLogic.IO.Type
import JsonLogic.Json
import JsonLogic.Operation (defaultOperations)
import JsonLogic.Operation.Array
import JsonLogic.Operation.Boolean
import JsonLogic.Operation.Data
import JsonLogic.Operation.Misc
import JsonLogic.Operation.Numeric
import JsonLogic.Operation.String
import Prelude hiding (all, and, any, filter, log, map, max, min, or, sum, (!!), (&&), (*), (+), (-), (/), (/=), (<), (<=), (==), (>), (>=), (||))

-- Initial environment with only "+" defined
createEnvIO :: OperationsIO -> Json -> JsonLogicEnvIO
createEnvIO fs = JLEnvIO (M.union fs defaultOperationsIO)

-- Default operators
defaultOperationsIO :: M.Map String FunctionIO
defaultOperationsIO = M.map (\f -> undefined) defaultOperations --TODO
