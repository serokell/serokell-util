-- | Benchmark related utils.

module Serokell.Util.Bench
       ( getWallTime
       , getCpuTime
       , ElapsedTime (..)
       , measureTime
       , measureTime_
       , perSecond
       ) where

import           Control.Monad.Trans (MonadIO (liftIO))
import           Data.Text.Buildable (Buildable (build))
import           Data.Time.Units     (Nanosecond, TimeUnit, convertUnit)
import           Formatting          (bprint, shown, (%))
import           System.Clock        (Clock (..), TimeSpec, diffTimeSpec,
                                      getTime, toNanoSecs)

-- | Get current wall-clock time as any time unit.
getWallTime :: (MonadIO m, TimeUnit a) => m a
getWallTime = timeSpecToUnit <$> getTime' Realtime

-- | Get current CPU time as any time unit.
getCpuTime :: (MonadIO m, TimeUnit a) => m a
getCpuTime = timeSpecToUnit <$> getTime' ProcessCPUTime

timeSpecToUnit
    :: TimeUnit a
    => TimeSpec -> a
timeSpecToUnit =
    convertUnit . (fromIntegral :: Integer -> Nanosecond) . toNanoSecs

-- | Data type describing time passed during execution of something.
data ElapsedTime = ElapsedTime
    { elapsedCpuTime  :: TimeSpec
    , elapsedWallTime :: TimeSpec
    } deriving (Show)

instance Buildable ElapsedTime where
    build ElapsedTime{..} =
        bprint
            ("(CPU time = " % shown % ", wall time = " % shown % ")")
            elapsedCpuTime
            elapsedWallTime

getTime' :: MonadIO m => Clock -> m TimeSpec
getTime' = liftIO . getTime

-- | Run given action and measure how much time it took.
measureTime :: MonadIO m => m a -> m (ElapsedTime, a)
measureTime action = do
    cpuTimeBefore <- getTime' ProcessCPUTime
    wallTimeBefore <- getTime' Realtime
    res <- action
    wallTimeAfter <- getTime' Realtime
    cpuTimeAfter <- getTime' ProcessCPUTime
    return
        ( ElapsedTime
          { elapsedCpuTime = cpuTimeAfter `diffTimeSpec` cpuTimeBefore
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
