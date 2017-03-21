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

import           Control.Monad              ((>=>))
import           Control.Monad.Fail         (MonadFail (fail))
import           Data.Aeson                 (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types           (FromJSONKey (..),
                                             FromJSONKeyFunction (FromJSONKeyTextParser),
                                             ToJSONKey (..), toJSONKeyText)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Base64.URL as B64url
import           Data.Either.Combinators    (mapLeft)
import           Data.Hashable              (Hashable)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.Builder     (Builder, fromText)
import           Formatting                 (Format, later)
import           Prelude                    hiding (fail)

-- | Apply base64 encoding to strict ByteString.
encode :: BS.ByteString -> T.Text
encode = decodeUtf8 . B64.encode

-- | Decode base64-encoded ByteString.
decode :: T.Text -> Either T.Text BS.ByteString
decode = mapLeft T.pack . B64.decode . encodeUtf8

-- | Apply base64url encoding to strict ByteString.
encodeUrl :: BS.ByteString -> T.Text
encodeUrl = decodeUtf8 . B64url.encode

-- | Decode base64url-encoded ByteString.
decodeUrl :: T.Text -> Either T.Text BS.ByteString
decodeUrl = mapLeft T.pack . B64url.decode . encodeUtf8

-- | Construct Builder from bytestring formatting it in Base64.
formatBase64 :: BS.ByteString -> Builder
formatBase64 = fromText . encode

-- | Format which uses Base64 to print bytestring.
base64F :: Format r (BS.ByteString -> r)
base64F = later formatBase64

----------------------------------------------------------------------------
-- Aeson helpers
----------------------------------------------------------------------------

-- | Wrapper on top of ByteString with JSON serialization (in base64
-- encoding).
newtype JsonByteString = JsonByteString
    { getJsonByteString :: BS.ByteString
    } deriving (Show, Eq, Ord, Hashable)

instance ToJSON JsonByteString where
    toJSON = toJSON . encode . getJsonByteString

instance ToJSONKey JsonByteString where
    toJSONKey = toJSONKeyText (encode . getJsonByteString)

instance FromJSON JsonByteString where
    parseJSON = parseJSON >=> jsonBSParser

instance FromJSONKey JsonByteString where
    fromJSONKey = FromJSONKeyTextParser jsonBSParser

jsonBSParser :: MonadFail m => T.Text -> m JsonByteString
jsonBSParser = either (fail . T.unpack) (pure . JsonByteString) . decode

----------------------------------------------------------------------------
-- Deprecated
----------------------------------------------------------------------------

newtype JsonByteStringDeprecated = JsonByteStringDeprecated
    { getJsonByteStringDeprecated :: BS.ByteString
    }

instance ToJSON JsonByteStringDeprecated where
    toJSON = toJSON . encodeUrl . getJsonByteStringDeprecated

instance FromJSON JsonByteStringDeprecated where
    parseJSON =
        parseJSON >=>
        either (fail . T.unpack) (pure . JsonByteStringDeprecated) . decodeUrl
