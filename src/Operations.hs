module Operations where

import Data.Map as M
import Json

-- Initial environment with only "+" defined
createEnv :: [Operation] -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union (M.fromList fs) defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations = M.fromList [("+", plus)]

-- Operation type
type Operation = (String, Function)

-- Implementation for plus
plus :: Json -> FunctionResult
plus (JsonArray [JsonNumber x, JsonNumber y]) = Right $ JsonNumber $ x + y
plus _ = Left "Wrong number of arguments for +"
