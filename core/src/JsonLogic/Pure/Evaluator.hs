module JsonLogic.Pure.Evaluator (eval) where

import Control.Monad.Except
import Control.Monad.Identity
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import qualified JsonLogic.Pure.Type as T

type Result = Either String Json

type SubEvaluator = Rule -> Data -> Result

type Function = SubEvaluator -> Rule -> Data -> Result

type Operation = (String, Function)

eval :: [Operation] -> Rule -> Data -> Result
eval ops rule d = runIdentity $ E.eval (map fromOperation ops) rule d

fromOperation :: Operation -> T.Operation
fromOperation (s, f) = (s, fromFunction f)

fromFunction :: Function -> T.Function
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

fromResult :: Result -> T.Result
fromResult = liftEither

toSubEvaluator :: T.SubEvaluator -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

toResult :: T.Result -> Result
toResult = runIdentity . runExceptT
