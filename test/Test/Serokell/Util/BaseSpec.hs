{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Test.Serokell.Util.BaseSpec
       ( spec
       ) where

import           Data.Aeson                (decode, encode)
import qualified Data.ByteString           as BS
import           Data.Maybe                (fromJust)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           ((===))
import           Test.QuickCheck.Instances ()

import           Serokell.Arbitrary        ()
import qualified Serokell.Util.Base16      as C16
import qualified Serokell.Util.Base64      as C64

deriving instance Eq C64.JsonByteString
deriving instance Show C64.JsonByteString

spec :: Spec
spec =
    describe "Serialization" $ do
        describe "Indentity Properties" $ do
            prop "Base16" $
                \(a :: BS.ByteString) -> a === base16Mid a
            prop "Base64" $
                \(a :: BS.ByteString) -> a === base64Mid a
            prop "JSON Base64" $
                \(a :: C64.JsonByteString) -> a === base64JSONMid a

base16Mid,
    base64Mid :: BS.ByteString -> BS.ByteString
base16Mid = fromRight . C16.decode . C16.encode
base64Mid = fromRight . C64.decode . C64.encode

base64JSONMid :: C64.JsonByteString -> C64.JsonByteString
base64JSONMid = fromJust . decode . encode

fromRight :: Either a BS.ByteString -> BS.ByteString
fromRight (Left _)  = error "failed decoding to ByteString"
fromRight (Right b) = b
