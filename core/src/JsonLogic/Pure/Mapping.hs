module JsonLogic.Pure.Mapping where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as M
import JsonLogic.Pure.Type
import qualified JsonLogic.Type as T

toResult :: T.Result r Identity -> Result r
toResult = runIdentity . runExceptT

fromResult :: Result r -> T.Result r Identity
fromResult = liftEither

toSubEvaluator :: T.SubEvaluator Identity -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

fromSubEvaluator :: SubEvaluator -> T.SubEvaluator Identity
fromSubEvaluator s r d = fromResult $ s r d

toFunction :: T.Function r Identity -> Function r
toFunction f s r d = toResult $ f (fromSubEvaluator s) r d

fromFunction :: Function r -> T.Function r Identity
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

toOperation :: T.Operation Identity -> Operation
toOperation (s, f) = (s, toFunction f)

fromOperation :: Operation -> T.Operation Identity
fromOperation (s, f) = (s, fromFunction f)

toOperations :: T.Operations Identity -> Operations
toOperations = M.map toFunction

fromOperations :: Operations -> T.Operations Identity
fromOperations = M.map fromFunction

toEnv :: T.JsonLogicEnv_ Identity -> JsonLogicEnv
toEnv (T.JLEnv_ ops vars) = JLEnv (toOperations ops) vars

fromEnv :: JsonLogicEnv -> T.JsonLogicEnv_ Identity
fromEnv (JLEnv ops vars) = T.JLEnv_ (fromOperations ops) vars
