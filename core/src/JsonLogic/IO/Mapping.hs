module JsonLogic.IO.Mapping where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.IO.Type
import qualified JsonLogic.Type as T

toResult :: T.Result r IO -> Result r
toResult = runExceptT

fromResult :: Result r -> T.Result r IO
fromResult = ExceptT

toSubEvaluator :: T.SubEvaluator IO -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

fromSubEvaluator :: SubEvaluator -> T.SubEvaluator IO
fromSubEvaluator s r d = fromResult $ s r d

toFunction :: T.Function r IO -> Function r
toFunction f s r d = toResult $ f (fromSubEvaluator s) r d

fromFunction :: Function r -> T.Function r IO
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

toOperation :: T.Operation IO -> Operation
toOperation (s, f) = (s, toFunction f)

fromOperation :: Operation -> T.Operation IO
fromOperation (s, f) = (s, fromFunction f)

toOperations :: T.Operations IO -> Operations
toOperations = M.map toFunction

fromOperations :: Operations -> T.Operations IO
fromOperations = M.map fromFunction

toEnv :: T.JsonLogicEnv_ IO -> JsonLogicEnv
toEnv (T.JLEnv_ ops vars) = JLEnv (toOperations ops) vars

fromEnv :: JsonLogicEnv -> T.JsonLogicEnv_ IO
fromEnv (JLEnv ops vars) = T.JLEnv_ (fromOperations ops) vars
