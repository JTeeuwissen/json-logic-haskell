{-# LANGUAGE OverloadedLists #-}

-- |
-- Module      : JsonLogic.Operation.String
-- Description : Internal JsonLogic operations on strings
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Operation.String (stringOperations, cat, substr) where

import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Type

stringOperations :: Monad m => Operations m
stringOperations = [cat, substr]

-- String Operations
cat, substr :: Monad m => Operation m
cat = ("cat", evaluateCat)
substr = ("substr", evaluateSubstr)

evaluateCat :: Monad m => Function m Json
evaluateCat evaluator args vars = do
  res <- evaluator args vars
  case res of
    (JsonArray js) -> return $ JsonString $ foldMap stringify js
    json -> return $ JsonString $ stringify json

-- | Evaluate substr operation
evaluateSubstr :: Monad m => Function m Json
evaluateSubstr evaluator param vars = do
  res <- evaluator param vars
  JsonString <$> case res of
    -- Take everything from the index (can be negative)
    JsonArray [s, i] -> do
      str <- evaluateString evaluator s vars
      index <- evaluateInt evaluator i vars
      return $ alterSubstr drop index str
    -- Take a part of the substring between the two indexes
    JsonArray (s : startI : endI : _) -> do
      str <- evaluateString evaluator s vars
      startIndex <- evaluateInt evaluator startI vars
      endIndex <- evaluateInt evaluator endI vars
      return $ alterSubstr take endIndex $ alterSubstr drop startIndex str
    -- No proper indexing arguments given, return the full json string
    json -> evaluateString evaluator json vars
  where
    -- Takes part of the substring given a positive or negative index
    alterSubstr :: (Int -> String -> String) -> Int -> String -> String
    alterSubstr f index str
      | index >= 0 = f index str
      | otherwise = f (length str + index) str
