{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Arbitrary instances for Serokell datatypes
module Serokell.Arbitrary
       ( VariantNoBytes (..)
       , VariantOnlyBytes (..)
       ) where

import           Data.ByteString               as BS hiding (zip)
import qualified Data.HashMap.Lazy             as H (fromList)
import           Data.Vector                   (fromList)
import           GHC.Generics                  (Generic)
import           Test.QuickCheck               (Arbitrary (..), Gen, choose,
                                                genericShrink, frequency, oneof,
                                                sized)
import           Test.QuickCheck.Instances     ()

import           Serokell.Data.Variant.Variant (Variant (..))
import qualified Serokell.Util.Base64          as S

instance Arbitrary S.JsonByteString where
    arbitrary = S.JsonByteString <$> (arbitrary :: Gen BS.ByteString)

newtype VariantNoBytes = NoBytes
    { getVariant :: Variant
    } deriving (Show, Eq, Generic)

newtype VariantOnlyBytes = OnlyBytes
    { getVarBytes :: Variant
    } deriving (Show, Eq, Generic)

instance Arbitrary VariantOnlyBytes where
    arbitrary = OnlyBytes <$> oneof [VarBytes <$> arbitrary]
    shrink = genericShrink

instance Arbitrary VariantNoBytes where
    arbitrary = NoBytes <$> (sized $ \n -> genVariant False (n*50 + 1))
    shrink = genericShrink

instance Arbitrary Variant where
    arbitrary = sized $ \n -> genVariant True (n*50 + 1)
    shrink = genericShrink

-- | Generate something with at most N constructors. Say how many constructors
-- actually are in the generated 'Variant'. Because encoding 'VarBytes' into
-- JSON and then decoding it presents a problem - it becomes a 'VarString',
-- and it is difficult to tell whether it was decoded from a 'VarBytes' or
-- another 'VarString' - this generating function won't generate this
-- constructor unless given a true boolean flag.
genVariant :: Bool -> Int -> Gen Variant
genVariant b 1 = genFlatVariant b
genVariant b n = do
    frequency
        -- No reason for “3”, it just works well.
        [ (3, genFlatVariant b)
        -- The more constructors we have to generate, the more likely we are
        -- to choose something nested instead of something flat.
        , (truncate (logBase 2 (fromIntegral n) :: Double),
             oneof [genListVariant b n, genMapVariant b n])
        ]

genFlatVariant :: Bool -> Gen Variant
genFlatVariant bytes = oneof $
    [ pure VarNone
    , VarBool <$> arbitrary
    , VarInt <$> arbitrary
    , VarUInt <$> arbitrary
    , VarFloat <$> arbitrary
    , VarString <$> arbitrary
    ] ++ if bytes then [VarBytes <$> arbitrary]
                  else []

-- | Generate a list of variants, using at most N constructors in total.
genBoundedVariants :: Bool -> Int -> Gen [Variant]
genBoundedVariants _ 0 = return []
genBoundedVariants b n = do
    v_cons <- choose (1, n)
    (:) <$> genVariant b v_cons
        <*> genBoundedVariants b (n-v_cons)

genListVariant :: Bool -> Int -> Gen Variant
genListVariant b n = VarList . fromList <$> genBoundedVariants b (n-1)

genMapVariant :: Bool -> Int -> Gen Variant
genMapVariant b n = do
    keys <- genBoundedVariants b ((n-1) `div` 2)
    vals <- genBoundedVariants b ((n-1) `div` 2)
    -- Lengths of keys and vals may not match and so we would get less
    -- constructors due to truncation, but we don't care.
    return $ VarMap $ H.fromList $ zip keys vals
