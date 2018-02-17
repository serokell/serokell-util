-- | Base64 encoding/decoding.

module Serokell.Util.Base64
       ( encode
       , decode
       , encodeUrl
       , decodeUrl
       , formatBase64
       , base64F
       , JsonByteString (..)
       , JsonByteStringDeprecated (..)
       ) where

import Universum hiding (fail)

import Control.Monad (fail)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Types (FromJSONKey (..), FromJSONKeyFunction (FromJSONKeyTextParser),
                         ToJSONKey (..), toJSONKeyText)
import Data.Text.Lazy.Builder (Builder, fromText)
import Formatting (Format, later)

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64url

-- | Apply base64 encoding to strict ByteString.
encode :: ByteString -> Text
encode = decodeUtf8 . B64.encode

-- | Decode base64-encoded ByteString.
decode :: Text -> Either Text ByteString
decode = first toText . B64.decode . encodeUtf8

-- | Apply base64url encoding to strict ByteString.
encodeUrl :: ByteString -> Text
encodeUrl = decodeUtf8 . B64url.encode

-- | Decode base64url-encoded ByteString.
decodeUrl :: Text -> Either Text ByteString
decodeUrl = first toText . B64url.decode . encodeUtf8

-- | Construct Builder from bytestring formatting it in Base64.
formatBase64 :: ByteString -> Builder
formatBase64 = fromText . encode

-- | Format which uses Base64 to print bytestring.
base64F :: Format r (ByteString -> r)
base64F = later formatBase64

----------------------------------------------------------------------------
-- Aeson helpers
----------------------------------------------------------------------------

-- | Wrapper on top of ByteString with JSON serialization (in base64
-- encoding).
newtype JsonByteString = JsonByteString
    { getJsonByteString :: ByteString
    } deriving (Show, Eq, Ord, Hashable)

instance ToJSON JsonByteString where
    toJSON = toJSON . encode . getJsonByteString

instance ToJSONKey JsonByteString where
    toJSONKey = toJSONKeyText (encode . getJsonByteString)

instance FromJSON JsonByteString where
    parseJSON = parseJSON >=> jsonBSParser

instance FromJSONKey JsonByteString where
    fromJSONKey = FromJSONKeyTextParser jsonBSParser

jsonBSParser :: MonadFail m => Text -> m JsonByteString
jsonBSParser = either (fail . toString) (pure . JsonByteString) . decode

----------------------------------------------------------------------------
-- Deprecated
----------------------------------------------------------------------------

newtype JsonByteStringDeprecated = JsonByteStringDeprecated
    { getJsonByteStringDeprecated :: ByteString
    }

instance ToJSON JsonByteStringDeprecated where
    toJSON = toJSON . encodeUrl . getJsonByteStringDeprecated

instance FromJSON JsonByteStringDeprecated where
    parseJSON =
        parseJSON >=>
        either (fail . toString) (pure . JsonByteStringDeprecated) . decodeUrl
