-- Run using "cabal run SandBox"
module Main where

import Control.Monad.State

import JsonLogicIO as Impure
import JsonLogic as Pure
import Json

import JsonExamples

main :: IO ()
main = do
    putStrLn "---------------------\nPure without IO\n---------------------"
    jsonLogic jlRun >>= print
    putStrLn "---------------------\nImpure with IO\n---------------------"
    jsonLogicIO jlIoRun

-- Pure function without IO
jlRun :: JL [Maybe Json]
jlRun = do
    -- Execute plusJson and directly print
  res1 <- Pure.apply plusJson JsonNull
  -- Call customPlus before it is added!
  res2 <- Pure.apply customPlusJson JsonNull
  -- This is how a custom operator is added
  Pure.addOperation "customOperation" customOperation
  -- Now that the custom operation is added, try to execute it again!
  -- The value is first saved before printing
  res3 <- Pure.apply customPlusJson JsonNull
  -- Example of a log (Will not print anything to the console)
  res4 <- Pure.apply logJson JsonNull
  -- This function returns everything
  return [res1, res2, res3, res4]

-- Impure version with IO
jlIoRun :: JLIO ()
jlIoRun = do
  -- Execute plusJson and directly print
  Impure.apply plusJson JsonNull >>= liftIO . print
  -- Call customPlus before it is added!
  Impure.apply customPlusJson JsonNull >>= liftIO . print
  -- This is how a custom operator is added
  Impure.addOperation "customOperation" customOperation
  -- Now that the custom operation is added, try to execute it again!
  -- The value is first saved before printing
  res3 <- Impure.apply customPlusJson JsonNull
  liftIO $ print res3
  -- Example of a log
  Impure.apply logJson JsonNull >>= liftIO . print
