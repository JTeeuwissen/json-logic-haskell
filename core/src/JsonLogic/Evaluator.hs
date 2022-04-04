-- |
-- Module      : JsonLogic.Evaluator
-- Description : Internal JsonLogic evaluator
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Evaluator (apply) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M
import JsonLogic.JL
import JsonLogic.Json
import JsonLogic.Type

-- evaluate JsonLogic without bothering about monads
apply :: Monad m => Operations m -> Rule -> Json -> Result m Json
apply ops rule d = runReader (evalRule rule) $ JLEnv ops d

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
evalRule :: Monad m => Rule -> JL (Result m Json) m
evalRule o@(JsonObject rule) = do
  result <- sequenceA <$> traverseWithKey evalFunc rule
  -- An empty rule returns itself
  return $ M.foldr const o <$> result -- FIX: this only returns a single rule result. Maybe evaluate one.
evalRule (JsonArray rules) = do
  result <- sequenceA <$> mapM evalRule rules
  return $ JsonArray <$> result
evalRule x = return $ return x

evalFunc :: Monad m => String -> Json -> JL (Result m Json) m
evalFunc fName param = do
  ops <- getOperations
  vars <- getVariables
  function <- getFunction fName
  return $ case function of
    Nothing -> throwError $ UnrecognizedOperation fName
    Just f -> f (subEval ops) param vars

subEval :: Monad m => Operations m -> Rule -> Data -> Result m Json
subEval ops rule d = runReader (evalRule rule) $ JLEnv ops d
