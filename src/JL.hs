module JL where

import Control.Monad.Reader (Reader, asks)
import qualified Data.Map as M
import Json (Function, Json, JsonLogicEnv (functions, variables))

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = Reader JsonLogicEnv a

getFunction :: String -> JL (Maybe Function)
getFunction name = asks (M.lookup name . functions)

getVariables :: JL Json
getVariables = asks variables