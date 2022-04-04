module JsonLogic.Evaluator (apply) where

import Control.Monad.Except
import Data.Map as M
import JsonLogic.Json
import JsonLogic.Type

-- | Evaluate a rule
-- Evaluate an object or array, return other items.
apply :: Monad m => Operations m -> Rule -> Data -> Result m Json
apply ops (JsonObject rules) dat = case M.toList rules of
  [(fName, fRule)] -> do
    case M.lookup fName ops of
      Nothing -> throwError $ UnrecognizedOperation fName
      Just f -> f (apply ops) fRule dat
  -- A rule should always contain only one field
  _ -> throwError $ InvalidRule (M.keys rules)
apply ops (JsonArray rules) dat = do
  result <- mapM (\rule -> apply ops rule dat) rules
  return $ JsonArray result
apply _ x _ = return x
