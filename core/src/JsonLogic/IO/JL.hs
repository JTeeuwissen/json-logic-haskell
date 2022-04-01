module JsonLogic.IO.JL where

import JsonLogic.IO.Type
import qualified JsonLogic.JL as J
import JsonLogic.Json

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = J.JL a IO

getFunction :: String -> JL (Maybe Function)
getFunction = J.getFunction

getOperations :: JL Operations
getOperations = J.getOperations

getVariables :: JL Json
getVariables = J.getVariables
