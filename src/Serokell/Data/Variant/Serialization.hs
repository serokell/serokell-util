{-# LANGUAGE TupleSections #-}

-- | This module contains serialization logic for Variant type.
-- Feel free to add serialization to/from other formats if you need it.

module Serokell.Data.Variant.Serialization
       (
       ) where

import           Control.Monad                 (when)
import qualified Data.Aeson                    as Aeson
import           Data.Bifunctor                (bimap)
import           Data.Binary                   (Binary, Get, Put)
import qualified Data.Binary                   as B
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM hiding (HashMap)
import           Data.Int                      (Int64)
import           Data.SafeCopy                 (SafeCopy)
import           Data.Scientific               (floatingOrInteger)
import qualified Data.Serialize                as Cereal
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as TE
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as GM
import           System.IO.Unsafe              (unsafePerformIO)

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

genericGetVectorWith :: (VG.Vector v a) => Cereal.Get a -> Cereal.Get (v a)
{-# INLINE genericGetVectorWith #-}
genericGetVectorWith getter = do
    len64 <- Cereal.get :: Cereal.Get Int64
    when (len64 > fromIntegral (maxBound :: Int)) $
        fail "Host can't deserialize a Vector longer than (maxBound :: Int)"
    VG.replicateM (fromIntegral len64) getter

-- | Write a 'Data.Vector.Generic.Vector' using custom 'Putter' for
-- the vector's elements.
genericPutVectorWith :: (VG.Vector v a) => Cereal.Putter a -> Cereal.Putter (v a)
{-# INLINE genericPutVectorWith #-}
genericPutVectorWith putter v = do
    Cereal.put ((fromIntegral $ VG.length v) :: Int64)
    VG.mapM_ putter v

-- | Write a 'Data.Vector.Generic.Vector'.
genericPutVector :: (Cereal.Serialize a, VG.Vector v a) => Cereal.Putter (v a)
{-# INLINE genericPutVector #-}
genericPutVector = genericPutVectorWith Cereal.put

-- | Read a 'Data.Vector.Generic.Vector'.
genericGetVector :: (Cereal.Serialize a, VG.Vector v a) => Cereal.Get (v a)
{-# INLINE genericGetVector #-}
genericGetVector = genericGetVectorWith Cereal.get

instance (Cereal.Serialize a) => Cereal.Serialize (V.Vector a) where
    get = genericGetVector
    put = genericPutVector
    {-# INLINE get #-}
    {-# INLINE put #-}

instance Cereal.Serialize Text where
    put = Cereal.put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> Cereal.get

instance (Eq a, Hashable a, Cereal.Serialize a, Cereal.Serialize b) =>
         Cereal.Serialize (HashMap a b) where
    put = Cereal.put . HM.toList
    get = HM.fromList <$> Cereal.get

instance Cereal.Serialize Variant

instance SafeCopy Variant

--  —————————Binary serialization————————— --
-- Here we use Generic support, it should be good enough.
instance Binary Variant

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap k v) where
    get = fmap HM.fromList B.get
    put = B.put . HM.toList

-- This was taken from 'vector-binary-instances' to remove it as a dependency

-- | Boxed, generic vectors.
instance Binary a => Binary (V.Vector a) where
    put = genericPutBinVector
    get = genericGetBinVector
    {-# INLINE get #-}

-- | Deserialize vector using custom parsers.
genericGetBinVectorWith :: (VG.Vector v a, Binary a)
    => Get Int       -- ^ Parser for vector size
    -> Get a         -- ^ Parser for vector's element
    -> Get (v a)
{-# INLINE genericGetBinVectorWith #-}
genericGetBinVectorWith getN getA = do
    n <- getN
    v <- return $ unsafePerformIO $ GM.unsafeNew n
    let go 0 = return ()
        go i = do x <- getA
                  () <- return $ unsafePerformIO $ GM.unsafeWrite v (n-i) x
                  go (i-1)
    () <- go n
    return $ unsafePerformIO $ VG.unsafeFreeze v

-- | Generic put for anything in the G.Vector class which uses custom
--   encoders.
genericPutBinVectorWith :: (VG.Vector v a, Binary a)
    => (Int -> Put)  -- ^ Encoder for vector size
    -> (a   -> Put)  -- ^ Encoder for vector's element
    -> v a -> Put
{-# INLINE genericPutBinVectorWith #-}
genericPutBinVectorWith putN putA v = do
    putN (VG.length v)
    VG.mapM_ putA v

-- | Generic function for vector deserialization.
genericGetBinVector :: (VG.Vector v a, Binary a) => Get (v a)
{-# INLINE genericGetBinVector #-}
genericGetBinVector = genericGetBinVectorWith B.get B.get

-- | Generic put for anything in the G.Vector class.
genericPutBinVector :: (VG.Vector v a, Binary a) => v a -> Put
{-# INLINE genericPutBinVector #-}
genericPutBinVector = genericPutBinVectorWith B.put B.put
