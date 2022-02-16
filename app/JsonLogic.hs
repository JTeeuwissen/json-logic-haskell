module JsonLogic where

import Control.Monad.State
import Control.Monad (void, when)
import Data.Map as Map

import Json
import Operations

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) and IO at the same time.
-- Which would otherwise be a real pain!
type JL a = State JsonLogicEnv a

-- Create environment
jsonLogic :: JL () -> IO ()
jsonLogic jlRun = return $ evalState jlRun newEnv

-- TODO use this one, could not quickly fit this one in
addOperation :: String -> ([Json] -> Json) -> JL ()
addOperation funcName f = modify (\(JLEnv funcs vars) -> JLEnv (Map.insert funcName f funcs) vars)