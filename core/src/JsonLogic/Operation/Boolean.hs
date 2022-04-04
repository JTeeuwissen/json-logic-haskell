{-# LANGUAGE OverloadedLists #-}

module JsonLogic.Operation.Boolean (booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or) where

import Control.Monad.Except
import JsonLogic.Json
import JsonLogic.Operation.Primitive
import JsonLogic.Operation.Utils
import JsonLogic.Type
import Prelude hiding (all, and, any, filter, map, max, min, or, sum, (!!), (&&), (==), (||))
import qualified Prelude as P hiding (and, or)

booleanOperations :: Monad m => Operations m
booleanOperations = [if', (==), (===), (!=), (!==), (!), (!!), and, or]

if' :: Monad m => Operation m
if' = ("if", evaluateIf)

-- Implementation for bool -> bool -> bool operators
(==), (===), (!=), (!==), (!), (!!), and, or :: Monad m => Operation m
(==) = ("==", looseEquals)
(===) = ("==", evaluateLogic (P.==))
(!=) = ("!=", looseNotEquals)
(!==) = ("!=", evaluateLogic (P./=))
(!) = ("!", evaluateFalsey)
(!!) = ("!!", evaluateTruthy)
and = ("and", evaluateLogic (P.&&))
or = ("or", evaluateLogic (P.||))

evaluateIf :: Monad m => Function m Json
evaluateIf evaluator (JsonArray [c, x, y]) vars = do
  res <- evaluateBool evaluator c vars
  evaluator (if res then x else y) vars
evaluateIf _ _ _ = throwError "Wrong number of arguments for if"

-- Helper functions

evaluateLogic :: Monad m => (Bool -> Bool -> Bool) -> Function m Json
evaluateLogic operator evaluator (JsonArray [x', y']) vars = do
  x <- evaluateBool evaluator x' vars
  y <- evaluateBool evaluator y' vars
  return $ JsonBool $ x `operator` y
evaluateLogic _ _ _ _ = throwError "Wrong number of arguments for logic operator"

evaluateTruthy :: Monad m => Function m Json
evaluateTruthy evaluator json vars = JsonBool <$> evaluateBool evaluator (evaluateUnaryArgument json) vars

evaluateFalsey :: Monad m => Function m Json
evaluateFalsey evaluator json vars = JsonBool . not <$> evaluateBool evaluator (evaluateUnaryArgument json) vars

-- | Evaluate loose equals
looseEquals :: Monad m => Function m Json
looseEquals evaluator (JsonArray [x', y']) vars = do
  x <- evaluator x' vars
  y <- evaluator y' vars
  return $
    JsonBool $ looseEq x y
looseEquals _ _ _ = throwError "Wrong number of arguments for loose not equals operator"

looseNotEquals :: Monad m => Function m Json
looseNotEquals evaluator (JsonArray [x', y']) vars = do
  x <- evaluator x' vars
  y <- evaluator y' vars
  return $
    JsonBool $ not $ looseEq x y
looseNotEquals _ _ _ = throwError "Wrong number of arguments for loose not equals operator"

-- | See: https://github.com/gregsdennis/json-everything/blob/master/JsonLogic/JsonElementExtensions.cs#L117
-- See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
looseEq :: Json -> Json -> Bool
looseEq (JsonArray a) (JsonArray b) = a P.== b
looseEq (JsonObject a) (JsonObject b) = a P.== b
looseEq (JsonNumber a) (JsonNumber b) = a P.== b
looseEq (JsonString a) (JsonString b) = a P.== b
looseEq (JsonBool a) (JsonBool b) = a P.== b
looseEq JsonNull JsonNull = True
looseEq JsonNull _ = False
looseEq _ JsonNull = False
looseEq (JsonObject _) _ = False
looseEq _ (JsonObject _) = False
looseEq a@(JsonNumber _) (JsonArray b) = looseEq a (JsonString $ show b)
looseEq (JsonArray a) b@(JsonNumber _) = looseEq (JsonString $ show a) b
looseEq a@(JsonNumber _) b = a P.== numberify b
looseEq a b@(JsonNumber _) = numberify a P.== b
looseEq (JsonArray a) (JsonString b) = show a P.== b
looseEq (JsonString a) (JsonArray b) = a P.== show b
looseEq _ _ = False

-- See https://github.com/gregsdennis/json-everything/blob/master/JsonLogic/JsonElementExtensions.cs#L84
numberify :: Json -> Json
numberify (JsonString s) = JsonNumber (read s)
numberify n@(JsonNumber _) = n
numberify (JsonBool b) = JsonNumber $ toEnum $ fromEnum b
numberify _ = JsonNull
