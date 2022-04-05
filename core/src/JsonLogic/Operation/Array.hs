{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

-- |
-- Module      : JsonLogic.Operation.Array
-- Description : Internal JsonLogic operations on arrays
-- Copyright   : (c) Marien Matser, Gerard van Schie, Jelle Teeuwissen, 2022
-- License     : MIT
-- Maintainer  : jelleteeuwissen@hotmail.nl
-- Stability   : experimental
module JsonLogic.Operation.Array (arrayOperations, map, reduce, filter, all, none, some, merge, in') where

import Control.Monad
import qualified Data.List as L
import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Type
import Prelude hiding (all, filter, map)

arrayOperations :: Monad m => Operations m
arrayOperations = [map, reduce, filter, all, none, some, merge, in']

map, reduce, filter :: Monad m => Operation m
map = ("map", evaluateMap)
reduce = ("reduce", evaluateReduce)
filter = ("filter", evaluateFilter)

all, none, some :: Monad m => Operation m
all = ("all", evaluateArrayToBool (\case [] -> False; bools -> and bools))
none = ("none", evaluateArrayToBool (not . or))
some = ("some", evaluateArrayToBool or)

merge, in' :: Monad m => Operation m
merge = ("merge", evaluateMerge)
in' = ("in", evaluateIn)

-- Evaluation for map
evaluateMap :: Monad m => Function m Json
evaluateMap evaluator (JsonArray [xs, f]) vars = do
  xs' <- evaluateArray evaluator xs vars -- This is our data we evaluate
  JsonArray <$> mapM (evaluator f) xs'
evaluateMap _ _ _ = throw "Map received the wrong arguments"

evaluateReduce :: Monad m => Function m Json
evaluateReduce evaluator (JsonArray [arrayExp, reduceFunction, initalExp]) vars = do
  array <- evaluateArray evaluator arrayExp vars
  initial <- evaluator initalExp vars
  foldM (\acc cur -> evaluator reduceFunction (JsonObject [("current", cur), ("accumulator", acc)])) initial array
evaluateReduce _ _ _ = throw "Wrong number of arguments for reduce"

evaluateFilter :: Monad m => Function m Json
evaluateFilter evaluator (JsonArray [xs, f]) vars = do
  array <- evaluateArray evaluator xs vars
  filtered <- filterM (evaluateBool evaluator f) array
  return $ JsonArray filtered
evaluateFilter _ _ _ = throw "Wrong number of arguments for filter"

evaluateArrayToBool :: Monad m => ([Bool] -> Bool) -> Function m Json
evaluateArrayToBool operator evaluator (JsonArray [xs, f]) vars = do
  xs' <- evaluateArray evaluator xs vars -- This is our data we evaluate
  bools <- mapM (evaluateBool evaluator f) xs'
  return $ JsonBool $ operator bools
evaluateArrayToBool _ _ _ _ = throw "Map received the wrong arguments"

-- | Merge operations flattens the array in the top level
evaluateMerge :: Monad m => Function m Json
evaluateMerge evaluator params vars = do
  res <- evaluator params vars
  case res of
    (JsonArray js) -> return $ JsonArray $ foldr merge' [] js
    -- If we get a single item, it gets put in an array
    json -> return $ JsonArray [json]
  where
    merge' (JsonArray as) acc = as ++ acc
    merge' j acc = j : acc

evaluateIn :: Monad m => Function m Json
evaluateIn evaluator (JsonArray (sub : arr : _)) vars = do
  sub' <- evaluator sub vars
  arr' <- evaluator arr vars
  return $
    JsonBool $ case (sub', arr') of
      (el, JsonArray xs) -> el `elem` xs
      (JsonString substr, JsonString s) -> substr `L.isInfixOf` s
      _ -> False
evaluateIn _ _ _ = return $ JsonBool False
