module JsonLogic.IO.Evaluator (eval) where

import Control.Monad.Except
import qualified JsonLogic.Evaluator as E
import qualified JsonLogic.IO.Type as T
import JsonLogic.Json

type Result = IO (Either String Json)

type SubEvaluator = Rule -> Data -> Result

type Function = SubEvaluator -> Rule -> Data -> Result

type Operation = (String, Function)

eval :: [Operation] -> Rule -> Data -> Result
eval ops = E.eval (map fromOperation ops)

fromOperation :: Operation -> T.Operation
fromOperation (s, f) = (s, fromFunction f)

fromFunction :: Function -> T.Function
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

fromResult :: Result -> T.Result
fromResult = ExceptT

toSubEvaluator :: T.SubEvaluator -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

toResult :: T.Result -> Result
toResult = runExceptT
