-- Run using "cabal run SandBox"
module Main where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (void, when)

import JsonLogicIO as Impure
import JsonLogic as Pure
import Json

-- Create Json logic context and run the state
main :: IO ()
main = jsonLogicIO jlioIoRun

jlioIoRun :: JLIO ()
jlioIoRun = do
  -- Execute plusJson
  res1 <- apply plusJson JsonNull
  liftIO $ print res1
  -- Call customPlus before it is added!
  res2 <- apply customPlusJson JsonNull
  liftIO $ print res2
  -- This is how a custom operator is added
  Impure.addOperation "customOperation" customOperation
  -- modify (\(JLEnv funcs vars) -> JLEnv (Map.insert "customPlus" plus funcs) vars)
  -- Now that the custom operation is added, try to execute it again!
  res3 <- apply customPlusJson JsonNull
  liftIO $ print res3
  res4 <- apply logJson JsonNull
  liftIO $ print res4

-- Apply the rule to the data
apply :: Rule -> Data -> JLIO Json
apply (JsonObject members) ds = Map.foldrWithKey eval (pure JsonNull) members
  where
    eval :: String -> Json -> JLIO Json -> JLIO Json
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
    eval _ _ _ = undefined
apply json _ = do
  liftIO $ putStrLn $ "ERROR: Implementation missing for " ++ show json
  return JsonNull

-------------------------------------------
-- Example JSON objects
-- {"log": "apple"}
logJson :: Json
logJson = JsonObject $ Map.singleton "log" (JsonString "apple")

-- { "customPlus": [1, 2] }
customPlusJson :: Json
customPlusJson = JsonObject $ Map.singleton "customOperation" (JsonArray [JsonNumber 1, JsonNumber 2])

-- { "+": [1, 2] }
plusJson :: Json
plusJson = JsonObject $ Map.singleton "+" (JsonArray [JsonNumber 1, JsonNumber 2])

-- 'custom operator'
customOperation :: [Json] -> Json
customOperation [JsonNumber x, JsonNumber y] = JsonNumber $ x - y
customOperation _ = undefined