module Operations where

import Control.Monad.Except (MonadError (throwError))
import Data.Map as M
import Json

-- Initial environment with only "+" defined
createEnv :: Operations -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations =
  M.fromList
    [ -- Arithmetic
      (Operations.+),
      (Operations.-),
      (Operations.*),
      (Operations./),
      -- Comparison
      (Operations.<),
      (Operations.>),
      (Operations.<=),
      (Operations.>=),
      -- Logic
      (Operations.&&),
      (Operations.||),
      (Operations.!=),
      (Operations.==),
      -- Functions
      Operations.map,
      Operations.var
    ]

-- Operation type
type Operation = (String, Function)

-- Primitive evaluators
evaluateNumber :: SubEvaluator -> Rule -> Data -> Either String Double
evaluateNumber evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonNumber n -> return n
    j -> throwError $ "Invalid parameter type, was expecting number. Got: " ++ show j

evaluateBool :: SubEvaluator -> Rule -> Data -> Either String Bool
evaluateBool evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonBool b -> return b
    _ -> throwError "Invalid parameter type, was expecting boolean"

-- Function evaluators
evaluateMath :: (Double -> Double -> Double) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateMath operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateNumber evaluator x vars
  y' <- evaluateNumber evaluator y vars
  return $ JsonNumber $ x' `operator` y'
evaluateMath _ _ _ _ = throwError "Wrong number of arguments for math operator"

evaluateComparison :: (Double -> Double -> Bool) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateComparison operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateNumber evaluator x vars
  y' <- evaluateNumber evaluator y vars
  return $ JsonBool $ x' `operator` y'
evaluateComparison _ _ _ _ = throwError "Wrong number of arguments for comparison operator"

evaluateLogic :: (Bool -> Bool -> Bool) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateLogic operator evaluator (JsonArray [x, y]) vars = do
  x' <- evaluateBool evaluator x vars
  y' <- evaluateBool evaluator y vars
  return $ JsonBool $ x' `operator` y'
evaluateLogic _ _ _ _ = throwError "Wrong number of arguments for logic operator"

-- Evaluation for map
evaluateArrayElems :: SubEvaluator -> Rule -> Data -> Either String [Json]
evaluateArrayElems evaluator (JsonArray xs) vars = mapM (`evaluator` vars) xs
evaluateArrayElems _ _ _ = throwError "EvaluteArray called on Non array type"

evaluateMap :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateMap evaluator (JsonArray [xs, f]) vars = do
  xs' <- evaluateArrayElems evaluator xs vars -- This is our data we evaluate
  JsonArray <$> mapM (evaluator f) xs'
evaluateMap _ _ _ = throwError "Map received the wrong arguments"

evaluateVar :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateVar _ (JsonString s) vars = return vars
evaluateVar _ s vars = throwError $ "LOGIC: " ++ show s ++ "VARS: " ++ show vars

indexVar :: [String] -> Data -> Either String Json
indexVar [] vars = return vars
indexVar (x : xs) j@(JsonObject o) = case M.lookup x o of
  Nothing -> throwError $ "cannot find: '" ++ show x ++ "' in data: " ++ show j
  Just js -> indexVar xs js
indexVar xs json = throwError $ "invalid search var: " ++ show (unwords xs) ++ " in data: " ++ show json

-- Same definition as words at: https://github.com/ghc/ghc/blob/master/libraries/base/Data/OldList.hs
splitOnPeriod :: String -> [String]
splitOnPeriod "" = []
splitOnPeriod s = case dropWhile ('.' Prelude.==) s of
  "." -> []
  s' -> w : splitOnPeriod s''
    where
      (w, s'') = break ('.' Prelude.==) s'

-- Implementation for arithmetic operators

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

map :: Operation
map = ("map", evaluateMap)

var :: Operation
var = ("var", evaluateVar)
