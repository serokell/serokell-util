-- | Util for MonadBaseControl

{-# LANGUAGE FlexibleContexts #-}

module Serokell.Util.Base
       ( inCurrentContext
       ) where

import           Control.Monad               (void)
import           Control.Monad.Trans         (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))

-- | Remembers monadic context of an action and transforms it to `IO`.
-- Note that any changes in context would be lost.
inCurrentContext :: (MonadBaseControl IO m, MonadIO n) => m () -> m (n ())
inCurrentContext action =
    liftBaseWith $ \runInIO -> return . liftIO . void $ runInIO action
