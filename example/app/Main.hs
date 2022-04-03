module Main where

import JsonLogic.Json
import JsonLogic.Pure.Evaluator
import JsonLogic.Pure.Operation

-- | The main function
-- Perform simple power function
-- >>> :{main
-- 4
-- 5
-- :}
-- TODO
main :: IO ()
main = do
  putStrLn "First number:"
  first <- getLine
  putStrLn "Second number:"
  second <- getLine
  print $ evaluatorWithPow (read $ "{\"**\":[" ++ first ++ "," ++ second ++ "]}") JsonNull

-- | An evaluator that can evaluate operations with power (**).
evaluatorWithPow :: Rule -> Data -> Result Json
evaluatorWithPow = eval [powEvaluator]

-- | The power operation.
powEvaluator :: Operation
powEvaluator = ("**", powFunction)

-- | The power function.
powFunction :: Function Json
powFunction evaluator (JsonArray [base', expo']) vars = do
  base <- evaluateDouble evaluator base' vars
  expo <- evaluateDouble evaluator expo' vars
  return $ JsonNumber $ base ** expo
powFunction _ _ _ = Left "Wrong number of arguments for **"
