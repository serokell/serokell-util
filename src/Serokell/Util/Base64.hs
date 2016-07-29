-- | Base64 encoding/decoding.

module Serokell.Util.Base64
       ( encode
       , decode
       , JsonByteString (..)
       ) where

import           Control.Monad              ((>=>))
import           Data.Aeson                 (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64
import           Data.Either.Combinators    (mapLeft)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)

-- | Apply base64 encoding to strict ByteString.
encode :: BS.ByteString -> T.Text
encode = decodeUtf8 . B64.encode

-- | Decode base64-encoded ByteString.
decode :: T.Text -> Either T.Text BS.ByteString
decode = mapLeft T.pack . B64.decode . encodeUtf8

-- | Wrapper on top of ByteString with JSON serialization (in base64
-- encoding).
newtype JsonByteString = JsonByteString
    { getJsonByteString :: BS.ByteString
    }

instance ToJSON JsonByteString where
    toJSON = toJSON . encode . getJsonByteString

instance FromJSON JsonByteString where
    parseJSON =
        parseJSON >=> either (fail . T.unpack) (pure . JsonByteString) . decode
