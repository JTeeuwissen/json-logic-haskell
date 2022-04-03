module JsonLogic.IO.Evaluator (Result, SubEvaluator, Function, Operation, eval) where

import Control.Monad.Except
import qualified Data.Map as M
import qualified JsonLogic.Evaluator as E
import JsonLogic.IO.Operation (defaultOperations)
import qualified JsonLogic.IO.Type as T
import JsonLogic.Json

type Result = IO (Either String Json)

type SubEvaluator = Rule -> Data -> Result

type Function = SubEvaluator -> Rule -> Data -> Result

type Operation = (String, Function)

eval :: [Operation] -> Rule -> Data -> Result
eval ops = E.eval $ M.union (M.map fromFunction (M.fromList ops)) defaultOperations

fromFunction :: Function -> T.Function
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

fromResult :: Result -> T.Result
fromResult = ExceptT

toSubEvaluator :: T.SubEvaluator -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

toResult :: T.Result -> Result
toResult = runExceptT
