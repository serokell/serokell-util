{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Benchmark related utils.

module Serokell.Util.Bench
       ( getWallTime
       , getCpuTime
       , ElapsedTime (..)
       , measureTime
       , measureTime_
       , perSecond
       ) where

import Universum hiding (Buildable)

import Fmt (Buildable (build), (+||), (||+))
import System.Clock (Clock (..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import Time (KnownRat, Time, ns, toUnit)

-- | Get current wall-clock time as any time unit.
getWallTime :: forall unit m . (MonadIO m, KnownRat unit) => m (Time unit)
getWallTime = timeSpecToUnit <$> getTime' Realtime

-- | Get current CPU time as any time unit.
getCpuTime :: forall unit m . (MonadIO m, KnownRat unit) => m (Time unit)
getCpuTime = timeSpecToUnit <$> getTime' ProcessCPUTime

timeSpecToUnit :: forall unit . KnownRat unit => TimeSpec -> Time unit
timeSpecToUnit = toUnit @unit . ns . fromIntegral . toNanoSecs

-- | Data type describing time passed during execution of something.
data ElapsedTime = ElapsedTime
    { elapsedCpuTime  :: TimeSpec
    , elapsedWallTime :: TimeSpec
    } deriving (Show)

instance Buildable ElapsedTime where
    build ElapsedTime{..} =
        "(CPU time = "+||elapsedCpuTime||+", wall time = "+||elapsedWallTime||+")"

getTime' :: MonadIO m => Clock -> m TimeSpec
getTime' = liftIO . getTime

-- | Run given action and measure how much time it took.
measureTime :: MonadIO m => m a -> m (ElapsedTime, a)
measureTime action = do
    cpuTimeBefore  <- getTime' ProcessCPUTime
    wallTimeBefore <- getTime' Realtime
    res            <- action
    wallTimeAfter  <- getTime' Realtime
    cpuTimeAfter   <- getTime' ProcessCPUTime
    return
        ( ElapsedTime
          { elapsedCpuTime  = cpuTimeAfter  `diffTimeSpec` cpuTimeBefore
          , elapsedWallTime = wallTimeAfter `diffTimeSpec` wallTimeBefore
          }
        , res)

-- | Run given action and measure how much time it took, discarding
-- result of action.
measureTime_ :: MonadIO m => m a -> m ElapsedTime
measureTime_ = fmap fst . measureTime

-- | Given number of actions executed during some time, this function
-- calculates how much actions were executed per second (on average).
perSecond :: (Real a, Fractional b) => a -> TimeSpec -> b
perSecond n time =
    fromRational $
    toRational n / (fromIntegral (max 1 $ toNanoSecs time) * 1.0e9)
