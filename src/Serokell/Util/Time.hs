{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions for working with time
module Serokell.Util.Time
       ( mcs
       , ms
       , sec
       , minute
       , hour
       ) where

import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Universum

-- | Converts a specified time to 'Microsecond'.
mcs, ms, sec, minute, hour :: Int -> Microsecond
mcs    = fromMicroseconds . fromIntegral
ms     = fromMicroseconds . fromIntegral . (*) 1000
sec    = fromMicroseconds . fromIntegral . (*) 1000000
minute = fromMicroseconds . fromIntegral . (*) 60000000
hour   = fromMicroseconds . fromIntegral . (*) 3600000000
