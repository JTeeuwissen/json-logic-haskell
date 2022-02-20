module JsonLogic where

import Control.Monad.Reader (runReader)
import Data.Map as M (foldr, traverseWithKey)
import JL (JL, getFunction)
import Json
  ( Data,
    EvalError (EvalError, errorMessage, functionName, paramaters),
    EvalResult,
    Json (JsonNull),
    Rule,
  )
import Operations (Operation, createEnv)

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> EvalResult
eval ops rule d = runReader (evalRule rule) $ createEnv ops d

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
  function <- getFunction fName
  return $ case function of
    Nothing -> createEvalError "Function not found"
    Just f -> case f param of
      Left message -> createEvalError message
      (Right js) -> Right js
  where
    createEvalError message = Left $ EvalError {functionName = fName, paramaters = param, errorMessage = message}
