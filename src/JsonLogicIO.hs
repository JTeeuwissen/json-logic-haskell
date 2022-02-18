module JsonLogicIO where

import Data.Map as M
import Data.Functor ( (<&>) )

import Control.Monad.State
    ( modify, evalStateT, MonadIO(liftIO), MonadState(get), StateT )

import Json
import Operations ( createEnv )

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) and IO at the same time.
-- Which would otherwise be a real pain!
type JLIO a = StateT JsonLogicEnv IO a

-- Create environment (For now with JsonNull as variable)
jsonLogicIO :: [(String, Function)] -> JLIO a -> IO a
jsonLogicIO fs jlRun = evalStateT jlRun (createEnv fs JsonNull)

-- How to add an operator to the JsonLogic
addOperation :: String -> ([Json] -> Maybe Json) -> JLIO ()
addOperation funcName f = modify (\(JLEnv funcs vars) -> JLEnv (M.insert funcName f funcs) vars)

-- Add data to the environment once
addData :: Json -> JLIO ()
addData json = modify (\(JLEnv funcs _) -> JLEnv funcs json)

-- Apply the rule to the data
apply :: Rule -> Data -> JLIO (Maybe Json)
apply rule json = addData json >> evalJson rule

evalJson :: Json -> JLIO (Maybe Json)
-- If any of the members evaluate to nothing the entire map evaluates to Nothing
evalJson (JsonObject jDict) = do
    jDict' <- sequenceA <$> traverseWithKey evalFunc jDict :: JLIO (Maybe (M.Map String Json))
    -- Sloppy Implementation, because the jsonlogic object does not have several memebers
    -- We can fold and just keep the last item. Needs revision
    case jDict' of
        Nothing -> return Nothing
        Just jDict'' -> return $ pure $ M.foldr const JsonNull jDict''
-- Evaluate all the elements of the array, if any of them eval to Nothing
-- the whole Object evaluates to Nothing, otherwise put it back in array
evalJson (JsonArray js) = do
    js' <- sequenceA <$> traverse evalJson js :: JLIO (Maybe [Json])
    case js' of
        Nothing -> return Nothing
        Just _ -> return $ JsonArray <$> js'
-- Json that does not have inner Json that needs to get evaluated
evalJson j = return $ pure j

evalFunc :: String -> Json -> JLIO (Maybe Json)
-- TODO implement var
evalFunc "var" _ = undefined
evalFunc "log" (JsonArray (j:_)) = logging j
evalFunc "log" json = logging json
evalFunc fName arr@(JsonArray _) = do
    env <- get
    case M.lookup fName $ functions env of
        Nothing -> funcMissing fName >> return Nothing
        Just f  -> evalJson arr <&> (=<<) (\(JsonArray js) -> f js)
evalFunc fName json = do
    env <- get
    case M.lookup fName $ functions env of
        Nothing -> funcMissing fName >> return Nothing
        Just f  -> evalJson json <&> (=<<) (\j -> f [j])

logging :: Json -> JLIO (Maybe Json)
logging j = do
    liftIO (putStrLn $ "LOG:JsonLogic: " ++ show j)
    return $ pure j

funcError :: String -> [Json] -> JLIO ()
funcError func args = liftIO $ putStrLn $ "ERROR:JsonLogic: func '" ++ show func ++ "' args '" ++ show args ++ "'."

funcMissing :: String -> JLIO ()
funcMissing fName = liftIO $ putStrLn $ "ERROR:JsonLogic: function '" ++ fName ++ "' MISSING in environment. Use addOperator to add it."
