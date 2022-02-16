module Operations where

import Data.Map as Map
import Json

-- Initial environment with only "+" defined
newEnv :: JsonLogicEnv
newEnv = JLEnv (Map.fromList [("+", plus)])
  Map.empty -- Variables

-- Implementation for plus
plus :: [Json] -> Json
plus [JsonNumber x, JsonNumber y] = JsonNumber $ x + y
plus _ = undefined