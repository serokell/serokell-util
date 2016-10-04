{-# LANGUAGE TupleSections #-}

-- | This module contains serialization logic for Variant type.
-- Feel free to add serialization to/from other formats if you need it.

module Serokell.Data.Variant.Serialization
       (
       ) where

import qualified Data.Aeson                    as Aeson
import           Data.Bifunctor                (bimap)
import           Data.Binary                   (Binary)
import           Data.Binary.Orphans           ()
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM hiding (HashMap)
import qualified Data.MessagePack              as MP
import           Data.SafeCopy                 (SafeCopy)
import           Data.Scientific               (floatingOrInteger)
import qualified Data.Serialize                as Cereal
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as TE
import qualified Data.Vector                   as V
import           Data.Vector.Serialize         ()

import           Serokell.Data.Variant.Variant (VarMap, Variant (..))
import           Serokell.Util.Base64          (JsonByteString (JsonByteString))
import           Serokell.Util.Text            (show')

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
varMapToObject = HM.fromList . map (bimap show' Aeson.toJSON) . HM.toList

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
        HM.toList $
        v

--  —————————Cereal and SafeCopy serialization————————— --
-- This serialization is very simple: first byte is tag followed by actual value.
-- `decode . encode` should be `id`.

-- TODO: move it somewhere??
instance Cereal.Serialize Text where
    put = Cereal.put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> Cereal.get

instance (Eq a, Hashable a, Cereal.Serialize a, Cereal.Serialize b) =>
         Cereal.Serialize (HashMap a b) where
    put = Cereal.put . HM.toList
    get = HM.fromList <$> Cereal.get

instance Cereal.Serialize Variant

instance SafeCopy Variant

--  —————————MessagePack serialization————————— --
-- MessagePack data structure is very close to Variant. However, note that:
-- 1. We are using strange library where Object type doesn't cover all
--    possible objects. For example, there is only `Int` for integers.
--    So every integer number is converted to `Int` (which may be imprecise).
--    Decoding checks sign of input (like JSON).
-- 2. MessagePack distinguishes between Float and Double while we don't.
-- 3. ObjectExt can't be decoded.

instance MP.MessagePack Variant where
    toObject VarNone = MP.ObjectNil
    toObject (VarBool v) = MP.ObjectBool v
    toObject (VarInt v) = MP.ObjectInt $ fromIntegral v
    toObject (VarUInt v) = MP.ObjectInt $ fromIntegral v
    toObject (VarFloat v) = MP.ObjectDouble v
    toObject (VarBytes v) = MP.ObjectBin v
    toObject (VarString v) = MP.ObjectStr v
    toObject (VarList v) = MP.ObjectArray . fmap MP.toObject . V.toList $ v
    toObject (VarMap v) =
        MP.ObjectMap .
        fmap (bimap MP.toObject MP.toObject) . HM.toList $
        v
    fromObject MP.ObjectNil = pure VarNone
    fromObject (MP.ObjectBool v) = pure . VarBool $ v
    fromObject (MP.ObjectInt v) | v < 0 = pure . VarInt . fromIntegral $ v
                                | otherwise = pure . VarUInt . fromIntegral $ v
    fromObject (MP.ObjectFloat v) = pure . VarFloat . realToFrac $ v
    fromObject (MP.ObjectDouble v) = pure . VarFloat $ v
    fromObject (MP.ObjectStr v) = pure . VarString $ v
    fromObject (MP.ObjectBin v) = pure . VarBytes $ v
    fromObject (MP.ObjectArray v) = fmap (VarList . V.fromList)
                                  . mapM MP.fromObject
                                  $ v
    fromObject (MP.ObjectMap v) =
        fmap (VarMap . HM.fromList) .
        mapM
            (\(a,b) ->
                  (,) <$> MP.fromObject a <*> MP.fromObject b) $
        v
    fromObject (MP.ObjectExt _ _) = fail "Can't deserialize ObjectExt"

--  —————————Binary serialization————————— --
-- Here we use Generic support, it should be good enough.
instance Binary Variant
