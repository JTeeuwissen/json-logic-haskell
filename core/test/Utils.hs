-- | This module is to provide shorter names for the json constructors for the tests
module Utils where

import qualified Data.Map as M (fromList)
import Hedgehog.Internal.Property
import JsonLogic.Json (Json (..))
import Test.Tasty
import qualified Test.Tasty.Hedgehog as H

jNull :: Json
jNull = JsonNull

jBool :: Bool -> Json
jBool = JsonBool

jNum :: Double -> Json
jNum = JsonNumber

jStr :: String -> Json
jStr = JsonString

jArr :: [Json] -> Json
jArr = JsonArray

jObj :: [(String, Json)] -> Json
jObj = JsonObject . M.fromList

hTestProperty :: TestName -> Property -> TestTree
hTestProperty name = H.testPropertyNamed name (PropertyName name)
