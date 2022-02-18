module JsonLogic where

import Control.Monad.State ( modify, evalState, MonadState(get), State )
import Data.Map as Map
import Data.Functor ( (<&>) )

import Json
import Operations ( createEnv )

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = State JsonLogicEnv a

-- evaluate JsonLogic without bothering about monads
eval :: [(String, Function)] -> Rule -> Data -> Maybe Json
eval oper json d = evalState (apply json d) $ createEnv oper

-- Create environment
jsonLogic :: [(String, Function)] -> JL a -> IO a
jsonLogic fs jlRun = return $ evalState jlRun (createEnv fs)

-- Add an operation to the list of functions
addOperation :: String -> ([Json] -> Maybe Json) -> JL ()
addOperation funcName f = modify (\(JLEnv funcs vars) -> JLEnv (Map.insert funcName f funcs) vars)

-- Add data to the environment once
addData :: Json -> JL ()
addData json = modify (\(JLEnv funcs _) -> JLEnv funcs json)

-- Apply the rule to the data
apply :: Rule -> Data -> JL (Maybe Json)
apply rule json = addData json >> evalJson rule

evalJson :: Json -> JL (Maybe Json)
-- If any of the members evaluate to nothing the entire map evaluates to Nothing
evalJson (JsonObject jDict) = do
    -- traverse [Just x, Just y] -> Nothing
    jDict' <- sequenceA <$> traverseWithKey evalFunc jDict :: JL (Maybe (Map.Map String Json))
    -- Sloppy Implementation, because the jsonlogic object does not have several memebers
    -- We can fold and just keep the last item. Needs revision
    case jDict' of
        Nothing -> return Nothing
        Just jDict'' -> return $ pure $ Map.foldr const JsonNull jDict''
-- Evaluate all the elements of the array, if any of them eval to Nothing
-- the whole Object evaluates to Nothing, otherwise put it back in array
evalJson (JsonArray js) = do
    js' <- sequenceA <$> traverse evalJson js :: JL (Maybe [Json])
    case js' of
        Nothing -> return Nothing
        Just _ -> return $ JsonArray <$> js'
-- Json that does not have inner Json that needs to get evaluated
evalJson j = return $ pure j

evalFunc :: String -> Json -> JL (Maybe Json)
-- TODO implement var
evalFunc "var" _ = undefined
evalFunc "log" json = evalJson json
evalFunc fName arr@(JsonArray _) = do
    env <- get
    case Map.lookup fName $ functions env of
        Nothing -> return Nothing
        Just f  -> evalJson arr <&> (\res -> res >>= f . lst)
evalFunc fName json = do
    env <- get
    case Map.lookup fName $ functions env of
        Nothing -> return Nothing
        Just f  -> evalJson json <&> (\res -> res >>= \r -> f [r])
