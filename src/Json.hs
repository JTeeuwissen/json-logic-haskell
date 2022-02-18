module Json where

import qualified Data.Map as M

-- Same definition
type Rule = Json

type Data = Json

-- Same definition
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject (M.Map String Json)
  deriving (Eq, Show)

type Function = [Json] -> Maybe Json

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { functions :: M.Map String ([Json] -> Maybe Json), -- Either Json String), -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show _ = ""
