module JsonLogic.Operations where

import Control.Monad.Except (MonadError (throwError))
import Data.Map as M hiding (map)
import JsonLogic.Json
import JsonLogic.Utils
import Prelude hiding (map, (&&), (*), (+), (-), (/), (/=), (<), (<=), (==), (>), (>=), (||))
import qualified Prelude as P

-- Initial environment with only "+" defined
createEnv :: Operations -> Json -> JsonLogicEnv
createEnv fs = JLEnv (M.union fs defaultOperations)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations =
  M.fromList
    [ -- Arithmetic
      (+),
      (-),
      (*),
      (/),
      -- Comparison
      (<),
      (>),
      (<=),
      (>=),
      -- Logic
      (&&),
      (||),
      (!=),
      (==),
      -- Other
      var,
      map
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
    j -> throwError $ "Invalid parameter type, was expecting boolean. Got: " ++ show j

evaluateArray :: SubEvaluator -> Rule -> Data -> Either String [Json]
evaluateArray evaluator param vars = do
  res <- evaluator param vars
  case res of
    JsonArray xs -> return xs
    j -> throwError $ "Invalid parameter type, was expecting array. Got: " ++ show j

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
evaluateMap :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateMap evaluator (JsonArray [xs, f]) vars = do
  xs' <- evaluateArray evaluator xs vars -- This is our data we evaluate
  JsonArray <$> mapM (evaluator f) xs'
evaluateMap _ _ _ = throwError "Map received the wrong arguments"

-- Evaluates a var
evaluateVar :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateVar evaluator param vars = do
  res <- evaluator param vars
  -- Extracts default value from array if it is one
  let (j, def) = getJsonWithDefault res
  case j of
    JsonNull -> return vars -- Always First value
    JsonBool _ -> return def -- Always Second value
    JsonNumber n -> return $ returnOrDefault (indexArray n vars) def
    JsonString s -> return $ returnOrDefault (indexData s vars) def
    -- Default value is already extracted, cannot have nested list as var value.
    JsonArray js -> throwError $ "Cannot evaluate a var of type array, namely: " ++ show js
    JsonObject o -> throwError $ "Cannot evaluate a var of type object, namely: " ++ show o

-- Implementation for arithmetic operators

(+) :: Operation
(+) = ("+", evaluateMath (P.+))

(-) :: Operation
(-) = ("-", evaluateMath (P.-))

(*) :: Operation
(*) = ("*", evaluateMath (P.*))

(/) :: Operation
(/) = ("/", evaluateMath (P./))

-- Implementation for bool -> bool -> bool operators
(&&) :: Operation
(&&) = ("and", evaluateLogic (P.&&))

(||) :: Operation
(||) = ("or", evaluateLogic (P.||))

(==) :: Operation
(==) = ("==", evaluateLogic (P.==)) -- TODO proper equality implementation.

(!=) :: Operation
(!=) = ("!=", evaluateLogic (P./=))

-- Implementation for double -> double -> bool operators
(<) :: Operation
(<) = ("<", evaluateComparison (P.<))

(>) :: Operation
(>) = (">", evaluateComparison (P.>))

(<=) :: Operation
(<=) = ("<=", evaluateComparison (P.<=))

(>=) :: Operation
(>=) = (">=", evaluateComparison (P.>=))

map :: Operation
map = ("map", evaluateMap)

var :: Operation
var = ("var", evaluateVar)
