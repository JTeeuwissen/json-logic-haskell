-- |
-- Module      : JsonLogic.IO.Mapping
-- Description : Internal JsonLogic IO functions to map from exposed types to internal types and vice versa
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.IO.Mapping where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.IO.Type
import qualified JsonLogic.Type as T

toResult :: T.Result IO r -> Result r
toResult = runExceptT

fromResult :: Result r -> T.Result IO r
fromResult = ExceptT

toSubEvaluator :: T.SubEvaluator IO -> SubEvaluator
toSubEvaluator s r d = toResult $ s r d

fromSubEvaluator :: SubEvaluator -> T.SubEvaluator IO
fromSubEvaluator s r d = fromResult $ s r d

toFunction :: T.Function IO r -> Function r
toFunction f s r d = toResult $ f (fromSubEvaluator s) r d

fromFunction :: Function r -> T.Function IO r
fromFunction f s r d = fromResult $ f (toSubEvaluator s) r d

toOperation :: T.Operation IO -> Operation
toOperation (s, f) = (s, toFunction f)

fromOperation :: Operation -> T.Operation IO
fromOperation (s, f) = (s, fromFunction f)

toOperations :: T.Operations IO -> Operations
toOperations = M.map toFunction

fromOperations :: Operations -> T.Operations IO
fromOperations = M.map fromFunction
