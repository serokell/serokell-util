{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Variant type.

module Serokell.Data.Variant.Variant
       ( Variant (..)
       , VarList
       , VarMap
       ) where

import Universum

import qualified Formatting.Buildable as B
import GHC.Exts (IsList (..))

import Serokell.Util.Text (listBuilderJSONIndent, mapBuilder)

import qualified Data.Vector as V
import qualified Serokell.Util.Base16 as B16

type VarList = Vector Variant
type VarMap = HashMap Variant Variant

-- | Variant is intended to store arbitrary data in arbitrary
-- format. You are free to choose data layout.
data Variant
    = VarNone               -- ^ None, i. e. no value.
    | VarBool !Bool         -- ^ Boolean value.
    | VarInt !Int64         -- ^ Signed integer number.
    | VarUInt !Word64       -- ^ Unsigned integer number.
    | VarFloat !Double      -- ^ IEEE 754 double precision floating point number.
    | VarBytes !ByteString  -- ^ Raw bytes.
    | VarString !Text       -- ^ Unicode string.
    | VarList !VarList      -- ^ List of Variants.
    | VarMap !VarMap        -- ^ Map (with unique keys) from Variant to Variant.
    deriving (Show,Eq,Generic)

instance B.Buildable Variant where
    build VarNone       = "None"
    build (VarBool v)   = B.build v
    build (VarInt v)    = B.build v
    build (VarUInt v)   = B.build v
    build (VarFloat v)  = B.build v
    build (VarBytes v)  = B.build . B16.encode $ v
    build (VarString v) = B.build v
    build (VarList v)   = listBuilderJSONIndent 2 v
    build (VarMap v)    = mapBuilder . toPairs $ v

instance Hashable (Vector Variant) where
    hashWithSalt salt = V.foldr' (flip hashWithSalt) (hashWithSalt salt ())

instance Hashable Variant

instance IsString Variant where
    fromString = VarString . fromString

instance IsList Variant where
    type Item Variant = Variant
    toList (VarList v) = Universum.toList v
    toList _           = error "toList: not a list"
    fromList = VarList . fromList

instance NFData Variant
