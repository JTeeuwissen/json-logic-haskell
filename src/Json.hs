module Json where

import Data.Aeson
import Data.Aeson.Encode.Pretty
  ( Config (Config),
    Indent (Spaces),
    NumberFormat (Generic),
    encodePretty',
  )
import Data.ByteString.Lazy as DBL (toStrict)
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO (putStrLn)

-- A rule can be any kind of JSON value, but object will be evaluated.
type Rule = Json

-- Data can be any kind of JSON value.
type Data = Json

-- Json is a collection of possivle JSON values.
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject (M.Map String Json)
  deriving (Eq, Show)

-- Convert to aeson json format
instance ToJSON Json where
  toJSON JsonNull = Null
  toJSON (JsonBool b) = toJSON b
  toJSON (JsonNumber n) = toJSON n
  toJSON (JsonString s) = toJSON s
  toJSON (JsonArray js) = toJSON js
  toJSON (JsonObject o) = toJSON o

-- Pretty print the JSON
prettyPrintJson :: Json -> IO ()
prettyPrintJson = TIO.putStrLn . decodeUtf8 . DBL.toStrict . encodePretty' config
  where
    config = Config (Spaces 2) mempty Generic False

-- Subevaluator, with rule, its context and retulting json.
type SubEvaluator = Rule -> Data -> EvalResult

type Function = SubEvaluator -> Json -> FunctionResult

type Operations = M.Map String Function

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show (JLEnv _ vs) = "JLEnv " ++ show vs

data JLError = JLError
  { functionName :: String,
    errorMessage :: String
  }
  deriving (Show, Eq)

type EvalResult = Either JLError Json

type FunctionResult = Either JLError Json
