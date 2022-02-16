module JsonLogicIO where

import Control.Monad.State
import Control.Monad (void, when)

import Data.Map as Map
import Json
import Operations

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) and IO at the same time.
-- Which would otherwise be a real pain!
type JLIO a = StateT JsonLogicEnv IO a

-- Create environment
jsonLogicIO :: JLIO () -> IO ()
jsonLogicIO jlRun = Control.Monad.void (runStateT jlRun newEnv)

-- TODO use this one, could not quickly fit this one in
addOperation :: String -> ([Json] -> Json) -> JLIO ()
addOperation funcName f = modify (\(JLEnv funcs vars) -> JLEnv (Map.insert funcName f funcs) vars)
