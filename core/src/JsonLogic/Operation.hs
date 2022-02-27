module JsonLogic.Operation where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Fixed as F
import qualified Data.Map as M hiding (map)
import JsonLogic.Json
import JsonLogic.Operation.Filter
import JsonLogic.Operation.If
import JsonLogic.Operation.Primitive (evaluateArray, evaluateBool, evaluateNumber)
import JsonLogic.Operation.Var
import Prelude hiding (filter, map, max, min, sum, (&&), (*), (+), (-), (/), (/=), (<), (<=), (==), (>), (>=), (||))
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
      (%),
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
      map,
      if',
      filter,
      min,
      max,
      sum
    ]

-- Operation type
type Operation = (String, Function)

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

-- Adds the between operator to check whether a number is between two other numbers
evaluateBetween :: (Double -> Double -> Bool) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateBetween operator evaluator (JsonArray [x, y, z]) vars = do
  x' <- evaluateNumber evaluator x vars
  y' <- evaluateNumber evaluator y vars
  z' <- evaluateNumber evaluator z vars
  return $ JsonBool $ (x' `operator` y') P.&& (y' `operator` z')
-- The regular two value case of the operator
evaluateBetween operator evaluator json vars = evaluateComparison operator evaluator json vars

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

-- Evaluation for max/min
evaluateDoubleArray :: ([Double] -> Double) -> SubEvaluator -> Rule -> Data -> Either String Json
evaluateDoubleArray _ _ _ (JsonArray []) = throwError "Cant perform action on empty list"
evaluateDoubleArray operator evaluator (JsonArray arr) vars = do
  arr' <- mapM (\x -> evaluateNumber evaluator x vars) arr
  return $ JsonNumber $ operator arr'

-- Implementation for arithmetic operators

(+), (-), (*), (/), (%) :: Operation
(+) = ("+", evaluateMath (P.+))
(-) = ("-", evaluateMath (P.-))
(*) = ("*", evaluateMath (P.*))
(/) = ("/", evaluateMath (P./))
(%) = ("%", evaluateMath F.mod')

-- Implementation for bool -> bool -> bool operators
(&&), (||), (==), (!=) :: Operation
(&&) = ("and", evaluateLogic (P.&&))
(||) = ("or", evaluateLogic (P.||))
(==) = ("==", evaluateLogic (P.==)) -- TODO proper equality implementation.
(!=) = ("!=", evaluateLogic (P./=))

-- Implementation for double -> double -> bool operators
(<), (>), (<=), (>=) :: Operation
(<) = ("<", evaluateBetween (P.<))
(>) = (">", evaluateComparison (P.>))
(<=) = ("<=", evaluateBetween (P.<=))
(>=) = (">=", evaluateComparison (P.>=))

-- Implementation for other operators
map, var, if', filter, min, max, sum :: Operation
map = ("map", evaluateMap)
var = ("var", evaluateVar)
if' = ("if", evaluateIf)
filter = ("filter", evaluateFilter)
min = ("min", evaluateDoubleArray P.minimum)
max = ("max", evaluateDoubleArray P.maximum)
sum = ("sum", evaluateDoubleArray P.sum)
