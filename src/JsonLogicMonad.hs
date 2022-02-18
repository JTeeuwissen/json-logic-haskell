module JsonLogicMonad where

import Control.Monad.State (evalState, modify)
import Data.Map as M
import Json
import JsonLogic
import Operations (createEnv)

-- Create environment (For now with JsonNull as variables)
jsonLogic :: [(String, Function)] -> JL a -> IO a
jsonLogic fs jlRun = return $ evalState jlRun (createEnv fs JsonNull)

-- Add an operation to the list of functions
addOperation :: String -> ([Json] -> Maybe Json) -> JL ()
addOperation funcName f = modify (\(JLEnv funcs vars) -> JLEnv (M.insert funcName f funcs) vars)

-- Add data to the environment once
addData :: Json -> JL ()
addData json = modify (\(JLEnv funcs _) -> JLEnv funcs json)

-- Apply the rule to the data
apply :: Rule -> Data -> JL (Maybe Json)
apply rule json = addData json >> evalJson rule
