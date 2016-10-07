-- | Utilities for @Data.Binary@.

module Serokell.Util.Binary
       ( decodeFull
       ) where

import           Data.Binary (Binary, decodeOrFail)
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy (ByteString)

-- | Like 'decode', but ensures that the whole input has been consumed.
decodeFull :: Binary a => ByteString -> Either String a
decodeFull bs = case decodeOrFail bs of
    Left (_, _, err) -> Left ("decodeFull: " ++ err)
    Right (unconsumed, _, a)
        | BSL.null unconsumed -> Right a
        | otherwise -> Left "decodeFull: unconsumed input"
