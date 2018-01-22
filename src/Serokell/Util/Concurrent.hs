-- | Convenient versions of some functions from `Control.Concurrent`

module Serokell.Util.Concurrent
       ( modifyTVarS
       ) where

import Universum

-- | Atomically modifies given `TVar`, associating state of given `StateT` with
-- `TVar` entry.
-- TODO: maybe generalize to any container and monad? Define as operator?
modifyTVarS :: TVar s -> StateT s STM a -> STM a
modifyTVarS t st = do
    s <- readTVar t
    (a, s') <- runStateT st s
    writeTVar t s'
    return a
