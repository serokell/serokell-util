-- | Base16 encoding/decoding.

module Serokell.Util.Base16
       ( encode
       , decode
       , formatBase16
       , base16F
       ) where

import Universum

import Data.Text.Lazy.Builder (Builder, fromText)
import Formatting (Format, later, sformat, stext, (%))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE
import qualified Fmt as Fmt

-- | Apply base16 encoding to strict ByteString.
encode :: BS.ByteString -> Text
encode = Fmt.fmt . Fmt.hexF

-- | Decode base16-encoded ByteString.
decode :: Text -> Either Text BS.ByteString
decode = handleError . B16.decode . TE.encodeUtf8
  where
    handleError (res,rest)
      | BS.null rest = pure res
      | otherwise =
          Left $
          sformat
              ("suffix is not in base-16 format: " % stext)
              (TE.decodeUtf8 rest)

-- | Construct Builder from bytestring formatting it in Base16.
formatBase16 :: BS.ByteString -> Builder
formatBase16 = fromText . encode

-- | Format which uses Base16 to print bytestring.
base16F :: Format r (BS.ByteString -> r)
base16F = later formatBase16
