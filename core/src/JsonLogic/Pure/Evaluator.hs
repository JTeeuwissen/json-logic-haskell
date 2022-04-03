module JsonLogic.Pure.Evaluator (Result, SubEvaluator, Function, Operation, eval) where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.Json
import JsonLogic.Pure.Operation (defaultOperations)
import qualified JsonLogic.Pure.Type as T

type Result = Either String Json

type SubEvaluator = Rule -> Data -> Result

type Function = SubEvaluator -> Rule -> Data -> Result

type Operation = (String, Function)

eval :: [Operation] -> Rule -> Data -> Result
eval ops rule d = runIdentity $ E.eval (M.union (M.map fromFunction (M.fromList ops)) defaultOperations) rule d

fromFunction :: Function -> T.Function
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

fromResult :: Result -> T.Result
fromResult = liftEither

toSubEvaluator :: T.SubEvaluator -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

toResult :: T.Result -> Result
toResult = runIdentity . runExceptT
