module Evaluator where

import Context (Stack)
import Json (Json)
import Operations (Operations)
import Rule (Rule)

-- An evaluator without a stack, for the first expression.
type Evaluator = [Operation] -> Rule -> [Json] -> Json

-- An evaluator without a stack, for the first expression.
type EvaluatorIO = [Operation] -> Rule -> [Json] -> IO Json

-- An evaluator with an existing stack, for sub expressions.
type SubEvaluator = Stack -> Rule -> [Json] -> Json
