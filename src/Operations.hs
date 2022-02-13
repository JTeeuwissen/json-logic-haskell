module Operations where

import Context (Context, Stack)
import Evaluator (Evaluator)
import Json (Json (JsonNumber))

-- | Function type, part of an operation.
type Function =
  Evaluator -> -- The current evaluator, can be used to evaluate subrules.
  Stack -> -- The stack of the current rule, can be used to acced the (previous) environment.
  [Json] -> -- The parameters of the operation
  (Json, Maybe Context) -- The result of the operation and optionally a new context.

-- | The type of an operation
data Operation = Operation
  { -- | The name of the operation
    name :: String,
    -- | The operation function
    function :: Function
  }

defaultOperations :: [Operation]
defaultOperations =
  [ plus
  -- More operations here
  ]

plus :: Operation
plus = Operation "+" (\evaluator stack params -> (plus' params, Nothing))
  where
    plus' :: [Json] -> Json
    plus' [JsonNumber l, JsonNumber r] = JsonNumber $ l + r
    plus' _ = error "Wrong number of parameters for +"
