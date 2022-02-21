module JsonLogic where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReader)
import Data.Map as M (Map, foldr, fromList, traverseWithKey)
import JL (JL, getFunction, getOperations)
import Json
  ( Data,
    Function,
    Json (JsonNull, JsonObject),
    JsonLogicEnv (JLEnv),
    Result,
    Rule,
  )
import Operations (Operation, createEnv)

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> Result
eval ops rule d = runReader (evalRule rule) $ createEnv (M.fromList ops) d

-- | Evaluate a rule
-- Currently only evaluates the first rule, non recursive.
evalRule :: Rule -> JL Result
evalRule (JsonObject rule) = do
  result <- sequenceA <$> traverseWithKey evalFunc rule
  return $ M.foldr const JsonNull <$> result
evalRule x = (return . return) x

evalFunc :: String -> Json -> JL Result
evalFunc fName param = do
  ops <- getOperations
  function <- getFunction fName
  return $ case function of
    Nothing -> throwError $ "Function" ++ fName ++ "Not found"
    Just f -> f (subEval ops) param

subEval :: M.Map String Function -> Rule -> Data -> Result
subEval ops rule d = runReader (evalRule rule) $ JLEnv ops d
