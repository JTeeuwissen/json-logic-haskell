module JsonLogic.Aeson where

import Data.Aeson (ToJSON (toJSON), Value (Null))
-- import Data.Aeson.KeyMap (toMap)
-- import Data.Aeson.Key (toString)
import Data.Aeson.Encode.Pretty
  ( Config (Config),
    Indent (Spaces),
    NumberFormat (Generic),
    encodePretty',
  )
-- import Data.Scientific (toRealFloat)
import Data.ByteString.Lazy as DBL (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO (putStrLn)
import JsonLogic.Json (Json (JsonArray, JsonBool, JsonNull, JsonNumber, JsonObject, JsonString))

-- import Data.Vector (toList)
-- import Data.Text (unpack)
-- import qualified Data.Map as M (mapKeys, mapWithKey, Map, map)

-- Convert to aeson json format
instance ToJSON Json where
  toJSON JsonNull = Null
  toJSON (JsonBool b) = toJSON b
  toJSON (JsonNumber n) = toJSON n
  toJSON (JsonString s) = toJSON s
  toJSON (JsonArray js) = toJSON js
  toJSON (JsonObject o) = toJSON o

-- instance FromJSON Json where
--   parseJSON Null = return JsonLogic.Json.JsonNull
--   parseJSON (Bool b) = return $ JsonBool b
--   parseJSON (Number n) = return $ JsonNumber $ toRealFloat n
--   parseJSON (String s) = return $ JsonString $ unpack s
--   -- parseJSON (Array xs) = return $ JsonArray <$> map fromJSON $ toList xs
--   parseJSON (Object o) = do
--     return $ fromSuccess JsonNull (do
--       res <- mapM fromJSON $ toMap o
--       return (JsonObject $ M.mapKeys toString res))
--   parseJSON _ = undefined

-- Pretty print the JSON
prettyPrintJson :: Json -> IO ()
prettyPrintJson = TIO.putStrLn . decodeUtf8 . DBL.toStrict . encodePretty' config
  where
    config = Config (Spaces 2) mempty Generic False
