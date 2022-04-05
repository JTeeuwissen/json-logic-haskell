{-# LANGUAGE OverloadedLists #-}

module Main where

import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import JsonLogic.Pure.Operation
import JsonLogic.Pure.Type

-- | The main function
-- Perform simple power function
main :: IO ()
main = do
  putStrLn "First number:"
  first <- getLine
  putStrLn "Second number:"
  second <- getLine
  print $ evaluate (read first) (read second)

-- | Evaluate two numbers with pow operation using Json logic.
-- The two numbers are placed into an data object and given to the evaluator with the following logic:
-- {"**":[{"var":"base"}, {"var":"exp"}]}
--
-- >>> evaluate (read "3") (read "4")
-- Right 81.0
evaluate :: Json -> Json -> Result Json
evaluate base expo = applyWithPow (read "{\"**\":[{\"var\":\"base\"}, {\"var\":\"exp\"}]}") (JsonObject [("base", base), ("exp", expo)])

-- | An evaluator that can evaluate operations with power (**).
applyWithPow :: Rule -> Data -> Result Json
applyWithPow = apply [powOperation]

-- | The power operation.
-- Takes the power function and adds a name to it to create an operation.
powOperation :: Operation
powOperation = ("**", powFunction)

-- | The power function.
-- Takes an subevaluator, function arguments (in this case just a list) and data to pass through.
-- 1. tries to evaluate the arguments to double values
--  (as they might be Json logic evaluating to doubles, instead of direct numbers).
-- 2. if successful, returns the result of the power operation
powFunction :: Function Json
powFunction evaluator (JsonArray [base', expo']) vars = do
  base <- evaluateDouble evaluator base' vars
  expo <- evaluateDouble evaluator expo' vars
  return $ JsonNumber $ base ** expo
powFunction _ _ _ = throw "Wrong number of arguments for **"
