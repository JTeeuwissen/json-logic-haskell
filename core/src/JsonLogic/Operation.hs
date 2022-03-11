{- ORMOLU_DISABLE -}
module JsonLogic.Operation
  ( createEnv, defaultOperations,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    miscOperations, log, trace,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    objectOperations, var, missing, missingSome,
    stringOperations, cat, substr
  )
where
{- ORMOLU_ENABLE -}

import qualified Data.Map as M hiding (map)
import JsonLogic.Json
import JsonLogic.Operation.Array
import JsonLogic.Operation.Boolean
import JsonLogic.Operation.Misc
import JsonLogic.Operation.Numeric
import JsonLogic.Operation.Object
import JsonLogic.Operation.String
import Prelude hiding (all, and, any, filter, log, map, max, min, or, sum, (!!), (&&), (*), (+), (-), (/), (/=), (<), (<=), (==), (>), (>=), (||))

-- Initial environment with only "+" defined
createEnv :: Operations -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations =
  M.unions
    [ arrayOperations,
      booleanOperations,
      miscOperations,
      numericOperations,
      objectOperations,
      stringOperations
    ]
