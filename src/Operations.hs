module Operations where

import Data.Map as M
import Json

-- Initial environment with only "+" defined
createEnv :: [(String, Function)] -> JsonLogicEnv
createEnv fs = JLEnv (M.union (M.fromList fs) defaultOperations) JsonNull -- Variables

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations = M.fromList [("+", plus)]

-- Implementation for plus
plus :: [Json] -> Maybe Json
plus [JsonNumber x, JsonNumber y] = Just $ JsonNumber $ x + y
plus _ = Nothing
