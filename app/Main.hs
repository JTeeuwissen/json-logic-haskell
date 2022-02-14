-- Run using "cabal run SandBox"
module Main where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (void, when)

-- Same definition
type Rule = Json
type Data = Json

-- Same definition
data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject (Map.Map String Json)
  deriving (Eq, Show)

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) and IO at the same time.
-- Which would otherwise be a real pain!
type JL a = StateT JsonLogicEnv IO a

-- Contains the functions are variables our environment has currently
data JsonLogicEnv = JLEnv {
    functions :: Map.Map String ([Json] -> Json), -- All the operations (plus custom ones)
    variables :: Map.Map String Json -- Variables defined in rules
}

-- Cannot derive itself, so empty instance
instance Show JsonLogicEnv where
    show x = ""

-- Create Json logic context and run the state
main :: IO ()
main = Control.Monad.void (runStateT jlRun newEnv)

jlRun :: JL ()
jlRun = do
  -- Execute plusJson
  res1 <- apply plusJson JsonNull
  liftIO $ print res1
  -- Call customPlus before it is added!
  res2 <- apply customPlusJson JsonNull
  liftIO $ print res2
  -- Can be put in seperate function, but this is how a custom operator is added
  -- GOAL: rewrite to -> addOperation "customPlus" plusJson
  modify (\(JLEnv funcs vars) -> JLEnv (Map.insert "customPlus" plus funcs) vars)
  -- Now that the custom operation is added, try to execute it again!
  res3 <- apply customPlusJson JsonNull
  liftIO $ print res3
  res4 <- apply logJson JsonNull
  liftIO $ print res4

-- TODO use this one, could not quickly fit this one in
addOperation :: String -> ([Json] -> Json) -> JL ()
addOperation k f = modify (\(JLEnv funcs vars) -> JLEnv (Map.insert k f funcs) vars)

-- Apply the rule to the data
apply :: Rule -> Data -> JL Json
apply (JsonObject members) ds = Map.foldrWithKey eval (pure JsonNull) members
  where
    eval :: String -> Json -> JL Json -> JL Json
    -- Log is seperate handler, cannot use a function type for this
    eval "log" json _ = do
      liftIO $ putStrLn $ "LOGGER: " ++ show json
      return json
    -- For now only functions have arrays, this can ofcourse be extended
    eval k (JsonArray arr) _ = do
      -- Extract environment from monad and get functions
      env <- get
      let funcs = (functions env :: Map.Map String ([Json] -> Json))
      -- Lookup key from functions and apply if found
      case Map.lookup k funcs of
        Just f -> return $ f arr
        Nothing -> do
-- Another important feature from this implementation
-- Instead of throwing errors, we can log the error and continue, maybe add flag to state to indicate invalid
          liftIO $ putStrLn $ "ERROR: function \"" ++ k ++ "\" missing from environment!"
          return JsonNull
apply json _ = do
  liftIO $ putStrLn $ "ERROR: Implementation missing for " ++ show json
  return JsonNull

-- Initial environment with only "+" defined
newEnv :: JsonLogicEnv
newEnv = JLEnv (Map.fromList [("+", plus)])
  Map.empty -- Variables

-- { "+": [1, 2] }
plusJson :: Json
plusJson = JsonObject $ Map.singleton "+" (JsonArray [JsonNumber 1, JsonNumber 2])

-- Implementation for plus
plus :: [Json] -> Json
plus [JsonNumber x, JsonNumber y] = JsonNumber $ x + y
plus [_, _] = undefined

-- {"log": "apple"}
logJson :: Json
logJson = JsonObject $ Map.singleton "log" (JsonString "apple")

-- { "customPlus": [1, 2] }
customPlusJson :: Json
customPlusJson = JsonObject $ Map.singleton "customPlus" (JsonArray [JsonNumber 1, JsonNumber 2])