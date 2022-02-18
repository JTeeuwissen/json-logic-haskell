module Operations where

import Context (Context, Stack)
import Evaluator (SubEvaluator)
import Json (Json (JsonNumber))

-- | Function type, part of an operation.
type Function =
  SubEvaluator -> -- The current evaluator, can be used to evaluate subrules.
  Stack -> -- The stack of the current rule, can be used to acced the (previous) environment.
  [Json] -> -- The parameters of the operation
  ReturnValue -- The result of the operation and optionally a new context.

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
plus = Operation "+" plusFunction
  where
    plusFunction :: Function
    plusFunction evaluator stack [l, r] = case [evaluator stack l [], evaluator stack r []] of
      [JsonNumber l, JsonNumber r] -> JsonNumber (l + r)
      _ -> error "Invalid parameters for +"
    plusFunction evaluator stack _ = error "Wrong number of parameters for +"

createEasy [JsonNumber l, JsonNumber r] -> JsonNumber (l + r)