module JsonLogic.IO.JL where

import Control.Monad.Reader (Reader, asks)
import qualified Data.Map as M
import JsonLogic.IO.Type
import JsonLogic.Json

type JLIO a = Reader JsonLogicEnvIO a

getFunctionIO :: String -> JLIO (Maybe FunctionIO)
getFunctionIO name = asks (M.lookup name . operations)

getOperationsIO :: JLIO OperationsIO
getOperationsIO = asks operations

getVariablesIO :: JLIO Json
getVariablesIO = asks variables
