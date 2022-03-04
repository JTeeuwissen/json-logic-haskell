module JsonLogic.Operation.Negation where

import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Operation.Utils

evaluateTruthy :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateTruthy evaluator json vars = JsonBool <$> evaluateBool evaluator (evaluateUnaryArgument json) vars

evaluateFalsey :: SubEvaluator -> Rule -> Data -> Either String Json
evaluateFalsey evaluator json vars = JsonBool . not <$> evaluateBool evaluator (evaluateUnaryArgument json) vars
