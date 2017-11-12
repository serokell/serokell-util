{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Memory units.

module Serokell.Data.Memory.Units
       (
         -- | Type class
         MemoryUnit (..)

         -- | Concrete types
       , Byte
       , Kilobyte
       , Megabyte
       , Gigabyte
       , Terabyte

         -- | Pretty printing
       , unitBuilder
       , memory

         -- | Helpers
       , toBytes
       , fromBytes
       , convertUnit
       ) where

import Control.DeepSeq (NFData)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable (Typeable)
import Formatting (bprint, stext, (%))
import qualified Formatting as Fmt
import GHC.Generics (Generic)

import Serokell.Util.Text (showFixedPretty')

import Test.QuickCheck (Arbitrary)

class Integral unit => MemoryUnit unit  where
    -- | This value is n iff (1 :: unit) is n bytes.
    bytesMultiplier :: Proxy unit -> Integer

-- | Convert given memory unit into integer representing bytes.
toBytes
    :: forall unit.
       MemoryUnit unit
    => unit -> Integer
toBytes mu = toInteger mu * bytesMultiplier proxy
  where
    proxy :: Proxy unit
    proxy = Proxy

-- | Convert given number of bytes into memory unit, flooring value if
-- necessary.
fromBytes
    :: forall unit.
       MemoryUnit unit
    => Integer -> unit
fromBytes bytes = fromInteger $ bytes `div` bytesMultiplier proxy
  where
    proxy :: Proxy unit
    proxy = Proxy

-- | Conversion between memory units.
convertUnit :: (MemoryUnit a, MemoryUnit b) => a -> b
convertUnit = fromBytes . toBytes

-- | Construct Text Builder.
unitBuilder :: MemoryUnit unit => unit -> Builder
unitBuilder n@(toBytes -> bytes)
  | bytes == 0 = "0"
  | bytes < 0 = mconcat ["-", unitBuilder n]
  | otherwise = bprint (stext % " " % stext) (showFixedPretty' 3 value) suffix
  where
    suffixes = ["B", "KiB", "MiB", "GiB", "TiB", "Pib", "EiB", "ZiB", "YiB"]
    bytesDouble :: Double
    bytesDouble = realToFrac bytes
    order = min (length suffixes - 1) (floor $ logBase 2 bytesDouble / 10)
    suffix = suffixes !! order
    value = bytesDouble / realToFrac ((2 :: Integer) ^ (order * 10))

-- | Formatter for `formatting` library.
memory :: MemoryUnit unit => Fmt.Format r (unit -> r)
memory = Fmt.later unitBuilder

pow10 :: Num n => Int -> n
pow10 = (10 ^)

newtype Byte =
    Byte Integer
    deriving (Show,Eq,Num,Typeable,Integral,Real,Enum,Ord,Generic,Arbitrary)

instance NFData Byte
instance MemoryUnit Byte where
    bytesMultiplier Proxy = pow10 0

newtype Kilobyte =
    Kilobyte Integer
    deriving (Show,Eq,Num,Typeable,Integral,Real,Enum,Ord,Generic,Arbitrary)

instance NFData Kilobyte
instance MemoryUnit Kilobyte where
    bytesMultiplier Proxy = pow10 3

-- P.S. Feel free to add more.

newtype Megabyte =
    Megabyte Integer
    deriving (Show,Eq,Num,Typeable,Integral,Real,Enum,Ord,Generic,Arbitrary)

instance NFData Megabyte
instance MemoryUnit Megabyte where
    bytesMultiplier Proxy = pow10 6

newtype Gigabyte =
    Gigabyte Integer
    deriving (Show,Eq,Num,Typeable,Integral,Real,Enum,Ord,Generic,Arbitrary)

instance NFData Gigabyte
instance MemoryUnit Gigabyte where
    bytesMultiplier Proxy = pow10 9

newtype Terabyte =
    Terabyte Integer
    deriving (Show,Eq,Num,Typeable,Integral,Real,Enum,Ord,Generic,Arbitrary)

instance NFData Terabyte
instance MemoryUnit Terabyte where
    bytesMultiplier Proxy = pow10 12
