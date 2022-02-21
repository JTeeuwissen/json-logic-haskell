module JsonLogic where

import Control.Monad.Except
import Control.Monad.Reader
-- import Control.Monad.Reader (lift, runReader)
import Data.Map as M
-- import JL (JL, getFunction, getOperations)
import Json

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

-- Primitive evaluators
evaluateNumber :: Json -> JL Double
evaluateNumber (JsonNumber n) = return n
evaluateNumber o@(JsonObject _) = do
  jsonRes <- evalRule o
  case jsonRes of
    JsonNumber n -> return n
    json -> throwError $ show o ++ "did not evaluate to a number, but to: " ++ show json
evaluateNumber j = throwError $ "Invalid parameter type, was expecting number, got " ++ show j

-- Function evaluators
evaluateMath :: (Double -> Double -> Double {-CreateError ->-}) -> Json -> JL Json
evaluateMath operator (JsonArray [x, y]) = do
  x' <- evaluateNumber x
  y' <- evaluateNumber y
  return $ JsonNumber $ x' `operator` y'
evaluateMath _ _ = throwError "Wrong number of arguments for math operator"

-- (+) :: Operation
(+) :: (String, Json -> JL Json)
(+) = ("+", evaluateMath (Prelude.+))

createEnv :: Map String Function -> Data -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- createOperation :: String -> ({-CreateError ->-} Json -> JL Json) -> Operation
-- createOperation name f = (name, f)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations =
  M.fromList
    [ -- Arithmetic
      (JsonLogic.+)
      -- (Operations.-),
      -- (Operations.*),
      -- (Operations./),
      -- -- Comparison
      -- (Operations.<),
      -- (Operations.>),
      -- (Operations.<=),
      -- (Operations.>=),
      -- -- Logic
      -- (Operations.&&),
      -- (Operations.||),
      -- (Operations.!=),
      -- (Operations.==)
    ]

-- Operation type
type Operation = (String, Function)
