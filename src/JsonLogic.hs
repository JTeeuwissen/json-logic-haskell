module JsonLogic where

import Control.Monad.State ( evalState, MonadState(get), State )
import Data.Map as M
import Data.Functor ( (<&>) )

import Json
import Operations ( createEnv )

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = State JsonLogicEnv a

-- evaluate JsonLogic without bothering about monads
eval :: [(String, Function)] -> Rule -> Data -> Maybe Json
eval oper json d = evalState (evalJson json) $ createEnv oper d

evalJson :: Json -> JL (Maybe Json)
-- If any of the members evaluate to nothing the entire map evaluates to Nothing
evalJson (JsonObject jDict) = do
    -- traverse [Just x, Just y] -> Nothing
    jDict' <- sequenceA <$> traverseWithKey evalFunc jDict :: JL (Maybe (M.Map String Json))
    -- Sloppy Implementation, because the jsonlogic object does not have several memebers
    -- We can fold and just keep the last item. Needs revision
    case jDict' of
        Nothing -> return Nothing
        Just jDict'' -> return $ pure $ M.foldr const JsonNull jDict''
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
    case M.lookup fName $ functions env of
        Nothing -> return Nothing
        Just f  -> evalJson arr <&> (=<<) (\(JsonArray js) -> f js)
evalFunc fName json = do
    env <- get
    case M.lookup fName $ functions env of
        Nothing -> return Nothing
        Just f  -> evalJson json <&> (=<<) (\j -> f [j])
