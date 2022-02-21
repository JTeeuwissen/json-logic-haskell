module Operations where

import Data.Map as M
import JL
import Json

-- createEnv :: Map String Function -> Data -> JsonLogicEnv
-- createEnv fs = JLEnv (M.union fs defaultOperations)

-- -- createOperation :: String -> (Json -> JL Json) -> Operation
-- -- createOperation name f = (name, f)

-- -- Default operators
-- defaultOperations :: M.Map String Function
-- defaultOperations =
--   M.fromList
--     [ -- Arithmetic
--       (Operations.+),
--       (Operations.-),
--       (Operations.*),
--       (Operations./),
--       -- Comparison
--       (Operations.<),
--       (Operations.>),
--       (Operations.<=),
--       (Operations.>=),
--       -- Logic
--       (Operations.&&),
--       (Operations.||),
--       (Operations.!=),
--       (Operations.==)
--     ]

-- evaluateBool :: Json -> JL Bool
-- evaluateBool _ _ (JsonBool b) = Right b
-- evaluateBool err evaluator (JsonObject o) = do
--   res <- evaluator o JsonNull
--   case res of
--     JsonBool b -> Right b
--     _ -> Left $ err "Invalid parameter type, was expecting boolean"
-- evaluateBool err _ _ = Left $ err "Invalid parameter type, was expecting boolean"

-- -- Function evaluators
-- evaluateMath :: (Double -> Double -> Double) -> Json -> JL Double
-- evaluateMath operator err evaluator (JsonArray [x, y]) = do
--   x' <- evaluateNumber err evaluator x
--   y' <- evaluateNumber err evaluator y
--   return $ JsonNumber $ x' `operator` y'
-- evaluateMath _ err _ _ = Left $ err "Wrong number of arguments for math operator"

-- evaluateComparison :: (Double -> Double -> Bool) -> Json -> JL Bool
-- evaluateComparison operator evaluator err (JsonArray [x, y]) = do
--   x' <- evaluateNumber evaluator err x
--   y' <- evaluateNumber evaluator err y
--   return $ JsonBool $ x' `operator` y'
-- evaluateComparison _ err _ _ = Left $ err "Wrong number of arguments for comparison operator"

-- evaluateLogic :: (Bool -> Bool -> Bool) -> Json -> JL Bool
-- evaluateLogic operator evaluator err (JsonArray [x, y]) = do
--   x' <- evaluateBool evaluator err x
--   y' <- evaluateBool evaluator err y
--   return $ JsonBool $ x' `operator` y'
-- evaluateLogic _ err _ _ = Left $ err "Wrong number of arguments for logic operator"

-- -- Implementation for arithmetic operators

-- (+) :: Operation
-- (+) = createOperation "+" $ evaluateMath (Prelude.+)

-- (-) :: Operation
-- (-) = createOperation "-" $ evaluateMath (Prelude.-)

-- (*) :: Operation
-- (*) = createOperation "*" $ evaluateMath (Prelude.*)

-- (/) :: Operation
-- (/) = createOperation "/" $ evaluateMath (Prelude./)

-- -- Implementation for bool -> bool -> bool operators
-- (&&) :: Operation
-- (&&) = createOperation "and" $ evaluateLogic (Prelude.&&)

-- (||) :: Operation
-- (||) = createOperation "or" $ evaluateLogic (Prelude.||)

-- (==) :: Operation
-- (==) = createOperation "==" $ evaluateLogic (Prelude.==) -- TODO proper equality implementation.

-- (!=) :: Operation
-- (!=) = createOperation "!=" $ evaluateLogic (Prelude./=)

-- -- Implementation for double -> double -> bool operators
-- (<) :: Operation
-- (<) = createOperation "<" $ evaluateComparison (Prelude.<)

-- (>) :: Operation
-- (>) = createOperation ">" $ evaluateComparison (Prelude.>)

-- (<=) :: Operation
-- (<=) = createOperation "<=" $ evaluateComparison (Prelude.<=)

-- (>=) :: Operation
-- (>=) = createOperation ">=" $ evaluateComparison (Prelude.>=)
