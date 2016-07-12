-- | Convenient versions of some functions from `Control.Concurrent`

module Serokell.Util.Concurrent
       ( threadDelay
       ) where

import qualified Control.Concurrent  as Concurrent
import           Control.Monad.Trans (MonadIO (liftIO))
import           Data.Time.Units     (TimeUnit (toMicroseconds))

-- | Convenient version of Control.Concurrent.threadDelay which takes any time-unit and operates in any MonadIO
threadDelay
    :: (MonadIO m, TimeUnit unit)
    => unit -> m ()
threadDelay = liftIO . Concurrent.threadDelay . fromIntegral . toMicroseconds
