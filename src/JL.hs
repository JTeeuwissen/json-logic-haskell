module JL where

import Control.Monad.Reader (Reader, asks)
import qualified Data.Map as M
import Json (Function, Json, JsonLogicEnv (operations, variables), Operations)

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = Reader JsonLogicEnv a

getFunction :: String -> JL (Maybe Function)
getFunction name = asks (M.lookup name . operations)

getOperations :: JL Operations
getOperations = asks operations

getVariables :: JL Json
getVariables = asks variables
