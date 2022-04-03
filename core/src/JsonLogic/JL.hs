module JsonLogic.JL where

import Control.Monad.Reader (Reader, asks)
import qualified Data.Map as M
import JsonLogic.Json
import JsonLogic.Type
import qualified JsonLogic.Type as T

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a m = Reader (JsonLogicEnv m) a

getFunction :: Monad m => String -> JL (Maybe (Function m Json)) m
getFunction name = asks (M.lookup name . T.operations)

getOperations :: Monad m => JL (Operations m) m
getOperations = asks T.operations

getVariables :: Monad m => JL Json m
getVariables = asks T.variables
