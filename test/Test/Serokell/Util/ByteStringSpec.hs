{-# LANGUAGE ScopedTypeVariables #-}

module Test.Serokell.Util.ByteStringSpec
       ( spec
       ) where

import Universum

import Data.Aeson (decode, encode)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))
import Test.QuickCheck.Instances ()

import Serokell.Arbitrary ()

import qualified Serokell.Util.Base16 as C16
import qualified Serokell.Util.Base64 as C64
import qualified Universum.Unsafe as Unsafe (fromJust)

spec :: Spec
spec =
    describe "Serialization" $
        describe "Indentity Properties" $ do
            prop "Base16" $
                \(a :: ByteString) -> a === base16Mid a
            prop "Base64" $
                \(a :: ByteString) -> a === base64Mid a
            prop "JSON Base64" $
                \(a :: C64.JsonByteString) -> a === base64JSONMid a

base16Mid,
    base64Mid :: ByteString -> ByteString
base16Mid = fromRightBS . C16.decode . C16.encode
base64Mid = fromRightBS . C64.decode . C64.encode

base64JSONMid :: C64.JsonByteString -> C64.JsonByteString
base64JSONMid = Unsafe.fromJust . decode . encode

fromRightBS :: Either a ByteString -> ByteString
fromRightBS = fromRight (error "failed decoding to ByteString")
