module Operations where

import Data.Map as Map
import Json

-- Initial environment with only "+" defined
newEnv :: JsonLogicEnv
newEnv = JLEnv (Map.fromList [("+", plus)]) JsonNull -- Variables

-- Implementation for plus
plus :: [Json] -> Maybe Json
plus [JsonNumber x, JsonNumber y] = Just $ JsonNumber $ x + y
plus _ = Nothing
