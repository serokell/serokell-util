-- | Base64 encoding/decoding.

module Serokell.Util.Base64
       ( encodeUrl
       , decodeUrl
       , JsonByteStringTODO (..)
       ) where

import           Control.Monad              ((>=>))
import           Data.Aeson                 (FromJSON (parseJSON),
                                             ToJSON (toJSON))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64url
import           Data.Either.Combinators    (mapLeft)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)

-- | Apply base64url encoding to strict ByteString.
encodeUrl :: BS.ByteString -> T.Text
encodeUrl = decodeUtf8 . B64url.encode

-- | Decode base64url-encoded ByteString.
decodeUrl :: T.Text -> Either T.Text BS.ByteString
decodeUrl = mapLeft T.pack . B64url.decode . encodeUtf8

-- | Wrapper on top of ByteString with JSON serialization (in base64url
-- encoding).
newtype JsonByteStringTODO = JsonByteStringTODO
    { getJsonByteStringTODO :: BS.ByteString
    }

instance ToJSON JsonByteStringTODO where
    toJSON = toJSON . encodeUrl . getJsonByteStringTODO

instance FromJSON JsonByteStringTODO where
    parseJSON =
        parseJSON >=>
        either (fail . T.unpack) (pure . JsonByteStringTODO) . decodeUrl
