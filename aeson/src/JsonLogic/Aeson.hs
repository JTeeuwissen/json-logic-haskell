module JsonLogic.Aeson (readJson, prettyPrintJson) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (..), decode)
import Data.Aeson.Encode.Pretty
  ( Config (Config),
    Indent (Spaces),
    NumberFormat (Generic),
    encodePretty',
  )
import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (toMap)
import qualified Data.ByteString.Lazy as DBL (toStrict)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map as M (mapKeys)
import Data.Scientific (toRealFloat)
import qualified Data.Text as DT (unpack)
import qualified Data.Text.Encoding as DTE (decodeUtf8)
import Data.Text.IO as TIO (putStrLn)
import Data.Vector (toList)
import JsonLogic.Json (Json (..))

-- Convert to aeson json format
instance ToJSON Json where
  toJSON JsonNull = Null
  toJSON (JsonBool b) = toJSON b
  toJSON (JsonNumber n) = toJSON n
  toJSON (JsonString s) = toJSON s
  toJSON (JsonArray js) = toJSON js
  toJSON (JsonObject o) = toJSON o

instance FromJSON Json where
  parseJSON Null = return JsonNull
  parseJSON (Bool b) = return $ JsonBool b
  parseJSON (Number n) = return $ JsonNumber $ toRealFloat n
  parseJSON (String s) = return $ JsonString $ DT.unpack s
  parseJSON (Array xs) = do
    res <- mapM parseJSON $ toList xs
    return $ JsonArray res
  parseJSON (Object o) = do
    res <- mapM parseJSON $ toMap o
    return (JsonObject $ M.mapKeys toString res)

-- | Read json from string and decode it into a Json object
readJson :: String -> Maybe Json
readJson s = decode $ BLU.fromString s

-- | Pretty print the JSON
prettyPrintJson :: Json -> IO ()
prettyPrintJson = TIO.putStrLn . DTE.decodeUtf8 . DBL.toStrict . encodePretty' config
  where
    config = Config (Spaces 2) mempty Generic False
