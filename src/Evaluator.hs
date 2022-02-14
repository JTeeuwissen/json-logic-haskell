module Evaluator where

import Context (Stack)
import Json (Json)
import Rule (Rule)

-- An evaluator without a stack, for the first expression.
type Evaluator = Rule -> [Json] -> Json

-- An evaluator with an existing stack, for sub expressions.
type SubEvaluator = Stack -> Rule -> [Json] -> Json
