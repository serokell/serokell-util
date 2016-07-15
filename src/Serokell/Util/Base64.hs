-- | Base64 encoding/decoding.

module Serokell.Util.Base64
       ( encode
       , decode
       ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import           Data.Either.Combinators (mapLeft)
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)

-- | Apply base64 encoding to strict ByteString.
encode :: BS.ByteString -> T.Text
encode = decodeUtf8 . B64.encode

-- | Decode base64-encoded ByteString.
decode :: T.Text -> Either T.Text BS.ByteString
decode = mapLeft T.pack . B64.decode . encodeUtf8
