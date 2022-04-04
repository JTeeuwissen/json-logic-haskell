-- |
-- Module      : JsonLogic.Pure.Mapping
-- Description : Internal JsonLogic Pure functions to map from exposed types to internal types and vice versa
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Pure.Mapping where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as M
import JsonLogic.Pure.Type
import qualified JsonLogic.Type as T

-- These functions are used to remove the monad transformer from the types.

toResult :: T.Result Identity r -> Result r
toResult = runIdentity . runExceptT

fromResult :: Result r -> T.Result Identity r
fromResult = liftEither

toSubEvaluator :: T.SubEvaluator Identity -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

fromSubEvaluator :: SubEvaluator -> T.SubEvaluator Identity
fromSubEvaluator s r d = fromResult $ s r d

toFunction :: T.Function Identity r -> Function r
toFunction f s r d = toResult $ f (fromSubEvaluator s) r d

fromFunction :: Function r -> T.Function Identity r
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

toOperation :: T.Operation Identity -> Operation
toOperation (s, f) = (s, toFunction f)

fromOperation :: Operation -> T.Operation Identity
fromOperation (s, f) = (s, fromFunction f)

toOperations :: T.Operations Identity -> Operations
toOperations = M.map toFunction

fromOperations :: Operations -> T.Operations Identity
fromOperations = M.map fromFunction
