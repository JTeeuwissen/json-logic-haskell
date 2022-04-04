# Core

The core JsonLogic evaluation package.
Allows for creating and adding custom operations to the evaluator in a pure and IO context.
See the example below for more information.

## Example
```hs
-- | The main function
-- Perform simple power function
main :: IO ()
main = do
  putStrLn "First number:"
  first <- getLine
  putStrLn "Second number:"
  second <- getLine
  print $ evaluate (read first) (read second)

-- | Evaluate two numbers with pow operation using json logic.
-- The two numbers are placed into an data object and given to the evaluator with the following logic:
-- {"**":[{"var":"base"}, {"var":"exp"}]}
-- >>> evaluate (read "3") (read "4")
-- 81
evaluate :: Json -> Json -> Result Json
evaluate base expo = evaluatorWithPow (read "{\"**\":[{\"var\":\"base\"}, {\"var\":\"exp\"}]}") (JsonObject [("base", base), ("exp", expo)])

-- | An evaluator that can evaluate operations with power (**).
evaluatorWithPow :: Rule -> Data -> Result Json
evaluatorWithPow = eval [powEvaluator]

-- | The power operation.
-- Takes the power function and adds a name to it to create an operation.
powEvaluator :: Operation
powEvaluator = ("**", powFunction)

-- | The power function.
-- Takes an subevaluator, function arguments (in this case just a list) and data to pass through.
-- 1. tries to evaluate the arguments to double values
--  (as they might be json logic evaluating to doubles, instead of direct numbers).
-- 2. if successful, returns the result of the power operation
powFunction :: Function Json
powFunction evaluator (JsonArray [base', expo']) vars = do
  base <- evaluateDouble evaluator base' vars
  expo <- evaluateDouble evaluator expo' vars
  return $ JsonNumber $ base ** expo
powFunction _ _ _ = Left "Wrong number of arguments for **"
```
