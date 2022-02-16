module Json where

import qualified Data.Map as Map

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
  | JsonObject (Map.Map String Json)
  deriving (Eq, Show)

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv {
    functions :: Map.Map String ([Json] -> Json), -- All the operations (plus custom ones)
    variables :: Map.Map String Json -- Variables defined in rules
}

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
    show x = ""