module Json where

import qualified Data.Map as Map

-- Same definition
type Rule = Json
type Data = Json

-- Same definition
data Json
  = JsonNull
  | JsonBool { bool :: Bool }
  | JsonNumber { val :: Double }
  | JsonString { str :: String }
  | JsonArray { lst :: [Json] }
  | JsonObject { dict :: Map.Map String Json }
  deriving (Eq, Show)

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv {
    functions :: Map.Map String ([Json] -> Maybe Json),-- Either Json String), -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
}

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
    show _ = ""
