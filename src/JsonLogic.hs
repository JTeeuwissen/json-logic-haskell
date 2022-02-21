module JsonLogic where

import Control.Monad.Except
import Control.Monad.Reader
-- import Control.Monad.Reader (lift, runReader)
import Data.Map as M
-- import JL (JL, getFunction, getOperations)

import JL
import Json
import Operations

-- ( CreateError,
--   Data,
--   EvalResult,
--   Function,
--   JLError (JLError),
--   Json (JsonArray, JsonBool, JsonNull, JsonNumber, JsonObject, JsonString),
--   JsonLogicEnv (JLEnv),
--   Rule,
-- )

-- import Operations (Operation, createEnv)
-- String can be our own ERROR type
eval :: [(String, Function)] -> Rule -> Data -> Either String Json
eval ops rule d = runExcept (runReaderT (evalRule rule) $ createEnv (M.fromList ops) d)

-- | Evaluate a rule
-- Currently only evaluates the first rule, non recursive.
evalRule :: Rule -> JL Json
evalRule (JsonObject rule) = head . M.elems <$> traverseWithKey evalFunc rule
evalRule json = return json

evalFunc :: String -> Json -> JL Json
evalFunc fName param = do
  function <- getFunction fName
  case function of
    Nothing -> throwError $ "Function '" ++ fName ++ "' Not found"
    Just f -> f param

createEnv :: Map String Function -> Data -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- (:: String -> (Json -> JL Json) -> Operation
-- (name f = (name, f)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations =
  M.fromList
    [ -- Arithmetic
      (JsonLogic.+),
      (JsonLogic.-),
      (JsonLogic.*),
      (JsonLogic./),
      -- Comparison
      (JsonLogic.<),
      (JsonLogic.>),
      (JsonLogic.<=),
      (JsonLogic.>=),
      -- Logic
      (JsonLogic.&&),
      (JsonLogic.||),
      (JsonLogic.!=),
      (JsonLogic.==)
    ]

-- Primitive evaluators
evaluateNumber :: Json -> JL Double
evaluateNumber (JsonNumber n) = return n
evaluateNumber o@(JsonObject _) = do
  jsonRes <- evalRule o
  case jsonRes of
    JsonNumber n -> return n
    json -> throwError $ show o ++ " did not evaluate to a number, but to: " ++ show json
evaluateNumber j = throwError $ "Invalid parameter type, was expecting number, got " ++ show j

evaluateBool :: Json -> JL Bool
evaluateBool (JsonBool b) = return b
evaluateBool o@(JsonObject _) = do
  res <- evalRule o
  case res of
    JsonBool b -> return b
    _ -> throwError "Invalid parameter type, was expecting boolean"
evaluateBool _ = throwError "Invalid parameter type, was expecting boolean"

-- Function evaluators
evaluateMath :: (Double -> Double -> Double) -> Json -> JL Json
evaluateMath operator (JsonArray [x, y]) = do
  x' <- evaluateNumber x
  y' <- evaluateNumber y
  return $ JsonNumber $ x' `operator` y'
evaluateMath _ _ = throwError "Wrong number of arguments for math operator"

evaluateComparison :: (Double -> Double -> Bool) -> Json -> JL Json
evaluateComparison operator (JsonArray [x, y]) = do
  x' <- evaluateNumber x
  y' <- evaluateNumber y
  return $ JsonBool $ x' `operator` y'
evaluateComparison _ _ = throwError "Wrong number of arguments for comparison operator"

evaluateLogic :: (Bool -> Bool -> Bool) -> Json -> JL Json
evaluateLogic operator (JsonArray [x, y]) = do
  x' <- evaluateBool x
  y' <- evaluateBool y
  return $ JsonBool $ x' `operator` y'
evaluateLogic _ _ = throwError "Wrong number of arguments for logic operator"

-- Implementation for arithmetic operators

type Operation = (String, Function)

(+) :: Operation
(+) = ("+", evaluateMath (Prelude.+))

(-) :: Operation
(-) = ("-", evaluateMath (Prelude.-))

(*) :: Operation
(*) = ("*", evaluateMath (Prelude.*))

(/) :: Operation
(/) = ("/", evaluateMath (Prelude./))

-- Implementation for bool -> bool -> bool operators
(&&) :: Operation
(&&) = ("and", evaluateLogic (Prelude.&&))

(||) :: Operation
(||) = ("or", evaluateLogic (Prelude.||))

(==) :: Operation
(==) = ("==", evaluateLogic (Prelude.==)) -- TODO proper equality implementation.

(!=) :: Operation
(!=) = ("!=", evaluateLogic (Prelude./=))

-- Implementation for double -> double -> bool operators
(<) :: Operation
(<) = ("<", evaluateComparison (Prelude.<))

(>) :: Operation
(>) = (">", evaluateComparison (Prelude.>))

(<=) :: Operation
(<=) = ("<=", evaluateComparison (Prelude.<=))

(>=) :: Operation
(>=) = (">=", evaluateComparison (Prelude.>=))
