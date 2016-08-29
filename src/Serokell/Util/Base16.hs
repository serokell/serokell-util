-- | Base16 encoding/decoding.

module Serokell.Util.Base16
       ( encode
       , decode
       ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Formatting             (sformat, stext, (%))

-- | Apply base16 encoding to strict ByteString.
encode :: BS.ByteString -> T.Text
encode = TE.decodeUtf8 . B16.encode

-- | Decode base16-encoded ByteString.
decode :: T.Text -> Either T.Text BS.ByteString
decode = handleError . B16.decode . TE.encodeUtf8
  where
    handleError (res,rest)
      | BS.null rest = pure res
      | otherwise =
          Left $
          sformat
              ("suffix is not in base-16 format: " % stext)
              (TE.decodeUtf8 rest)
