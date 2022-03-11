module JsonLogic.Operation.In where

import qualified Data.List as L
import JsonLogic.Json (Function, Json (JsonArray, JsonBool, JsonString))

evaluateIn :: Function
evaluateIn evaluator (JsonArray (sub : arr : _)) vars = do
  sub' <- evaluator sub vars
  arr' <- evaluator arr vars
  return $
    JsonBool $ case (sub', arr') of
      (el, JsonArray xs) -> el `elem` xs
      (JsonString substr, JsonString s) -> substr `L.isInfixOf` s
      _ -> False
evaluateIn _ _ _ = return $ JsonBool False
