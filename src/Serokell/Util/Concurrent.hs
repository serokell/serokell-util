-- | Convenient versions of some functions from `Control.Concurrent`

module Serokell.Util.Concurrent
       ( threadDelay
       , modifyTVarS
       ) where

import qualified Control.Concurrent          as Concurrent
import           Control.Concurrent.STM      (STM)
import           Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import           Control.Monad.State         (StateT, runStateT)
import           Control.Monad.Trans         (MonadIO (liftIO))
import           Data.Time.Units             (TimeUnit (toMicroseconds))


-- | Convenient version of Control.Concurrent.threadDelay which takes
-- any time-unit and operates in any MonadIO
threadDelay :: (MonadIO m, TimeUnit unit) => unit -> m ()
threadDelay = liftIO . Concurrent.threadDelay . fromIntegral . toMicroseconds

-- | Atomically modifies given `TVar`, associating state of given `StateT` with
-- `TVar` entry.
-- TODO: maybe generalize to any container and monad? Define as operator?
modifyTVarS :: TVar s -> StateT s STM a -> STM a
modifyTVarS t st = do
    s <- readTVar t
    (a, s') <- runStateT st s
    writeTVar t s'
    return a
