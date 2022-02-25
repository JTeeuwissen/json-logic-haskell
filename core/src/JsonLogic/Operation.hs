module JsonLogic.Operation where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map as M hiding (map)
import JsonLogic.Json
import JsonLogic.Operation.Filter
import JsonLogic.Operation.If
import JsonLogic.Operation.Primitive (evaluateArray, evaluateBool, evaluateNumber)
import JsonLogic.Operation.Var
import Prelude hiding (filter, map, (&&), (*), (+), (-), (/), (/=), (<), (<=), (==), (>), (>=), (||))
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
      map,
      if',
      filter
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

-- Implementation for arithmetic operators

(+), (-), (*), (/) :: Operation
(+) = ("+", evaluateMath (P.+))
(-) = ("-", evaluateMath (P.-))
(*) = ("*", evaluateMath (P.*))
(/) = ("/", evaluateMath (P./))

-- Implementation for bool -> bool -> bool operators
(&&), (||), (==), (!=) :: Operation
(&&) = ("and", evaluateLogic (P.&&))
(||) = ("or", evaluateLogic (P.||))
(==) = ("==", evaluateLogic (P.==)) -- TODO proper equality implementation.
(!=) = ("!=", evaluateLogic (P./=))

-- Implementation for double -> double -> bool operators
(<), (>), (<=), (>=) :: Operation
(<) = ("<", evaluateComparison (P.<))
(>) = (">", evaluateComparison (P.>))
(<=) = ("<=", evaluateComparison (P.<=))
(>=) = (">=", evaluateComparison (P.>=))

-- Implementation for other operators
map, var, if', filter :: Operation
map = ("map", evaluateMap)
var = ("var", evaluateVar)
if' = ("if", evaluateIf)
filter = ("filter", evaluateFilter)
