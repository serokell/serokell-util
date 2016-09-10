{-# LANGUAGE StandaloneDeriving #-}

-- | Arbitrary instances for Serokell datatypes
module Serokell.Arbitrary () where

import           Data.ByteString               as BS
import           Test.QuickCheck               (Arbitrary (..), Gen, oneof)
import           Test.QuickCheck.Instances     ()

import qualified Serokell.Data.Variant.Variant as S
import qualified Serokell.Util.Base64          as S

instance Arbitrary S.JsonByteString where
    arbitrary = S.JsonByteString <$> (arbitrary :: Gen BS.ByteString)

instance Arbitrary S.Variant where
    arbitrary =
        oneof
            [ pure S.VarNone
            , S.VarBool <$> arbitrary
            , S.VarInt <$> arbitrary
            , S.VarUInt <$> arbitrary
            , S.VarFloat <$> arbitrary
            , S.VarBytes <$> arbitrary
            , S.VarString <$> arbitrary
     --       , S.VarList <$> arbitrary
     --       , S.VarMap <$> (resize 100 arbitrary)
            ]
