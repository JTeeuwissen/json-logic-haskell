module JsonLogic where

import Control.Monad.State (MonadState (get), State, evalState)
import Data.Map as M (foldr, lookup, traverseWithKey)
import Json
  ( Data,
    EvalError (EvalError, errorMessage, functionName, paramaters),
    EvalResult,
    Json (JsonNull),
    JsonLogicEnv (functions),
    Rule,
  )
import Operations (Operation, createEnv)

-- Our monad type, contains the logicEnv
-- Now we can use JL (which holds our env) when we need it
type JL a = State JsonLogicEnv a

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> EvalResult
eval ops rule d = evalState (evalRule rule) $ createEnv ops d

-- | Evaluate a rule
-- Currently only evaluates the first rule, non recursive.
evalRule :: Rule -> JL EvalResult
evalRule rule = do
  jDict' <- sequenceA <$> traverseWithKey evalFunc rule
  return $ case jDict' of
    Left message -> Left message
    Right jDict'' -> return $ M.foldr const JsonNull jDict''

evalFunc :: String -> Json -> JL EvalResult
evalFunc fName param = do
  env <- get
  return $ case M.lookup fName $ functions env of
    Nothing -> createEvalError "Function not found"
    Just f -> case f param of
      Left message -> createEvalError message
      (Right js) -> Right js
  where
    createEvalError message = Left $ EvalError {functionName = fName, paramaters = param, errorMessage = message}
