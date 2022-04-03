module Main where

import JsonLogic.Json
import JsonLogic.Pure.Evaluator

main :: IO ()
main = do
  putStrLn "First number:"
  first <- getLine
  putStrLn "Second number:"
  second <- getLine
  print $ evaluatorWithPow (read first) (read second)

evaluatorWithPow :: Rule -> Data -> Result
evaluatorWithPow = eval [powEvaluator]

powEvaluator :: Operation
powEvaluator = ("**", undefined)
