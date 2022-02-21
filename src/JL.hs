module JL where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.Map as M
import Json

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = ReaderT JsonLogicEnv (Except String) a

getFunction :: String -> JL (Maybe Function)
getFunction name = asks (M.lookup name . operations)

getOperations :: JL Operations
getOperations = asks operations

getVariables :: JL Json
getVariables = asks variables

type Operations = (M.Map String Function)

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv
  { operations :: Operations, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
  show (JLEnv _ vs) = "JLEnv " ++ show vs

type Function = Data -> Rule -> Json

data JLError = JLError
  { functionName :: String,
    errorMessage :: String
  }
  deriving (Show, Eq)
