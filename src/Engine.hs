module Engine where

import Evaluator (Evaluator)
import Operations (Operation)

-- Note: seperated from Evaluator as operation needs evaluator and mutually recursive imports are a bad plan.
createEvaluator :: [Operation] -> Evaluator
createEvaluator = undefined
