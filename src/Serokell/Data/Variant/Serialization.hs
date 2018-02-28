{-# LANGUAGE TupleSections #-}

-- | This module contains serialization logic for Variant type.
-- Feel free to add serialization to/from other formats if you need it.

module Serokell.Data.Variant.Serialization
       (
       ) where

import Universum

import Data.Scientific (floatingOrInteger)

import Serokell.Data.Variant.Variant (VarMap, Variant (..))
import Serokell.Util.Base64 (JsonByteString (JsonByteString))

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM

--  —————————JSON serialization————————— --
-- Since there is no bijection between Variant and JSON Value, there are some
-- non-trivial things:
-- 1. decode . encode ≠ id. For example, (decode . encode) for VarBytes
--    will return VarString.
-- 2. Bytes are encoded in base64 encoding.
-- 3. If map contains key which is not a string, this key is converted to
--    string using Buildable instance. Usually you should avoid JSON
--    serialization for such maps.
-- 4. For numbers there is smart deserialization which tries to guess right
--    type (signed int/unsigned int/float).
--    If number is floating, VarFloat will be returned. For integer values
--    result type depends on sign (negative ⇒ Int, otherwise UInt).

varMapToObject :: VarMap -> Aeson.Object
varMapToObject = HM.fromList . map (bimap pretty Aeson.toJSON) . toPairs

instance Aeson.ToJSON Variant where
    toJSON VarNone       = Aeson.Null
    toJSON (VarBool v)   = Aeson.toJSON v
    toJSON (VarInt v)    = Aeson.toJSON v
    toJSON (VarUInt v)   = Aeson.toJSON v
    toJSON (VarFloat v)  = Aeson.toJSON v
    toJSON (VarBytes v)  = Aeson.toJSON . JsonByteString $ v
    toJSON (VarString v) = Aeson.toJSON v
    toJSON (VarList v)   = Aeson.toJSON v
    toJSON (VarMap v)    = Aeson.Object . varMapToObject $ v
    toEncoding VarNone       = Aeson.toEncoding Aeson.Null
    toEncoding (VarBool v)   = Aeson.toEncoding v
    toEncoding (VarInt v)    = Aeson.toEncoding v
    toEncoding (VarUInt v)   = Aeson.toEncoding v
    toEncoding (VarFloat v)  = Aeson.toEncoding v
    toEncoding (VarBytes v)  = Aeson.toEncoding . JsonByteString $ v
    toEncoding (VarString v) = Aeson.toEncoding v
    toEncoding (VarList v)   = Aeson.toEncoding v
    toEncoding (VarMap v)    = Aeson.toEncoding . varMapToObject $ v

instance Aeson.FromJSON Variant where
    parseJSON Aeson.Null = pure VarNone
    parseJSON (Aeson.Bool v) = pure . VarBool $ v
    parseJSON (Aeson.Number v) =
        pure . either VarFloat convertInt . floatingOrInteger $ v
      where
        convertInt :: Integer -> Variant
        convertInt i
          | i < 0 = VarInt $ fromIntegral i
          | otherwise = VarUInt $ fromIntegral i
    parseJSON (Aeson.String v) = pure . VarString $ v
    parseJSON (Aeson.Array v) = fmap VarList . mapM Aeson.parseJSON $ v
    parseJSON (Aeson.Object v) =
        fmap (VarMap . HM.fromList) .
        mapM
            (\(key,val) ->
                  (VarString key, ) <$> Aeson.parseJSON val) .
        toPairs $
        v
