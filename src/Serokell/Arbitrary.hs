{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Arbitrary instances for Serokell datatypes
module Serokell.Arbitrary () where

import           Data.ByteString               as BS hiding (zip)
import qualified Data.HashMap.Lazy             as H (fromList)
import           Data.Vector                   (fromList)
import           Test.QuickCheck               (Arbitrary (..), Gen, Small (..),
                                                choose, listOf, oneof, sized)
import           Test.QuickCheck.Instances     ()

import           Serokell.Data.Variant.Variant (Variant (..))
import qualified Serokell.Util.Base64          as S

instance Arbitrary S.JsonByteString where
    arbitrary = S.JsonByteString <$> (arbitrary :: Gen BS.ByteString)

instance Arbitrary Variant where
    arbitrary = do
        (Small size)  <- arbitrary :: Gen (Small Word)
        genVariant size

genVariant :: Word -> Gen Variant
genVariant 0 = genFlatVariant
  where
    genFlatVariant = oneof
                         [ pure VarNone
                         , VarBool <$> arbitrary
                         , VarInt <$> arbitrary
                         , VarUInt <$> arbitrary
                         , VarFloat <$> arbitrary
                         , VarBytes <$> arbitrary
                         , VarString <$> arbitrary
                         ]

genVariant n =
        oneof
            [ pure VarNone
            , VarBool <$> arbitrary
            , VarInt <$> arbitrary
            , VarUInt <$> arbitrary
            , VarFloat <$> arbitrary
            , VarBytes <$> arbitrary
            , VarString <$> arbitrary
            , VarList . fromList <$> listOf (genVariant (n - 1))
            , varMapGen
            ]
  where
    varMapGen =  do
        keys <- listOf (genVariant (n-1))
        vals <- listOf (genVariant (n-1))
        return $ VarMap $ H.fromList $ zip keys vals
