module JsonAeson where

import Data.Aeson
import Data.Aeson.Encode.Pretty
  ( Config (Config),
    Indent (Spaces),
    NumberFormat (Generic),
    encodePretty',
  )
import Data.ByteString.Lazy as DBL (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO (putStrLn)
import Json

-- Convert to aeson json format
instance ToJSON Json where
  toJSON JsonNull = Null
  toJSON (JsonBool b) = toJSON b
  toJSON (JsonNumber n) = toJSON n
  toJSON (JsonString s) = toJSON s
  toJSON (JsonArray js) = toJSON js
  toJSON (JsonObject o) = toJSON o

-- Pretty print the JSON
prettyPrintJson :: Json -> IO ()
prettyPrintJson = TIO.putStrLn . decodeUtf8 . DBL.toStrict . encodePretty' config
  where
    config = Config (Spaces 2) mempty Generic False
