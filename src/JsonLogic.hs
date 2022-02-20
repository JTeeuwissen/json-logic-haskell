module JsonLogic where

import Control.Monad.Reader (runReader)
import Data.Map as M (Map, foldr, fromList, traverseWithKey)
import JL (JL, getFunction, getOperations)
import Json
  ( Data,
    EvalError (EvalError, functionError, functionName),
    EvalResult,
    Function,
    FunctionError (FunctionError),
    Json (JsonNull),
    JsonLogicEnv (JLEnv),
    Rule,
    paramaters,
  )
import Operations (Operation, createEnv)

-- evaluate JsonLogic without bothering about monads
eval :: [Operation] -> Rule -> Data -> EvalResult
eval ops rule d = runReader (evalRule rule) $ createEnv (M.fromList ops) d

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
  ops <- getOperations
  return $ case function of
    Nothing -> createEvalError $ FunctionError "Function not found" Nothing
    Just f -> case f (subEval ops) param of
      Left message -> createEvalError message
      (Right js) -> Right js
  where
    createEvalError message = Left $ EvalError {functionName = fName, paramaters = param, functionError = message}

subEval :: M.Map String Function -> Rule -> Data -> EvalResult
subEval ops rule d = runReader (evalRule rule) $ JLEnv ops d
