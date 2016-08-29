{-# LANGUAGE TupleSections #-}

-- | This module contains serialization logic for Variant type.

module Serokell.Data.Variant.Serialization
       (
       ) where

import qualified Control.Monad.Fail            as Fail (MonadFail (fail))
import qualified Data.Aeson                    as Aeson
import           Data.Bifunctor                (bimap)
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM hiding (HashMap)
import           Data.SafeCopy                 (SafeCopy)
import           Data.Scientific               (floatingOrInteger)
import qualified Data.Serialize                as Cereal
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as TE
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

instance Cereal.Serialize Variant where
    put VarNone       = Cereal.putWord8 0
    put (VarBool v)   = Cereal.putWord8 1 >> Cereal.put v
    put (VarInt v)    = Cereal.putWord8 2 >> Cereal.put v
    put (VarUInt v)   = Cereal.putWord8 3 >> Cereal.put v
    put (VarFloat v)  = Cereal.putWord8 4 >> Cereal.put v
    put (VarBytes v)  = Cereal.putWord8 5 >> Cereal.put v
    put (VarString v) = Cereal.putWord8 6 >> Cereal.put v
    put (VarList v)   = Cereal.putWord8 7 >> Cereal.put v
    put (VarMap v)    = Cereal.putWord8 8 >> Cereal.put v
    get = do
        tag <- Cereal.getWord8
        case tag of
            0   -> pure VarNone
            1   -> VarBool <$> Cereal.get
            2   -> VarInt <$> Cereal.get
            3   -> VarUInt <$> Cereal.get
            4   -> VarFloat <$> Cereal.get
            5   -> VarBytes <$> Cereal.get
            6   -> VarString <$> Cereal.get
            7   -> VarList <$> Cereal.get
            8   -> VarMap <$> Cereal.get
            bad -> Fail.fail $ "bad tag " ++ show bad

instance SafeCopy Variant

-- TODO: msgpack, binary, whatever else…
