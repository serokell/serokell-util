{-# LANGUAGE TypeFamilies #-}

-- | Some useful functions to work with Data.Acid.

module Serokell.Util.AcidState
       (
         -- | Old helpers (TODO: move somewhere maybe?)
         exceptStateToUpdate
       , exceptStateToUpdateGeneric
       , readerToQuery
       , stateToUpdate

         -- | Utilities
       , createAndDiscardArchive
       , tidyLocalState

         -- | ExtendedState
       , ExtendedState (..)
       , closeExtendedState
       , extendedStateToAcid
       , openLocalExtendedState
       , openMemoryExtendedState
       , queryExtended
       , tidyExtendedState
       , updateExtended
       ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad.Reader       (Reader, asks, runReader)
import           Control.Monad.State        (State, runState, state)
import           Control.Monad.Trans        (MonadIO (liftIO))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Acid                  (AcidState, EventResult, EventState,
                                             IsAcidic, Query, QueryEvent,
                                             Update, UpdateEvent,
                                             closeAcidState, createArchive,
                                             createCheckpoint,
                                             openLocalStateFrom)
import           Data.Acid.Advanced         (query', update')
import           Data.Acid.Memory           (openMemoryState)
import           Data.Typeable              (Typeable)
import           System.Directory           (removeDirectoryRecursive)
import           System.FilePath            ((</>))

readerToQuery :: Reader s a -> Query s a
readerToQuery = asks . runReader

stateToUpdate :: State s a -> Update s a
stateToUpdate = state . runState

exceptStateToUpdate
    :: (Exception e)
    => ExceptT e (State s) a -> Update s a
exceptStateToUpdate = exceptStateToUpdateGeneric id

exceptStateToUpdateGeneric
  :: (Exception exc)
  => (e -> exc) -> ExceptT e (State s) a -> Update s a
exceptStateToUpdateGeneric toException u =
    state $
    runState $
    do res <- runExceptT u
       either (throw . toException) return res

-- | Archive unnecessary data (see createArchive docs for details) and
-- discard it. Works for local state.
createAndDiscardArchive :: MonadIO m => AcidState st -> FilePath -> m ()
createAndDiscardArchive st path =
    liftIO $ createArchive st >> removeDirectoryRecursive (path </> "Archive")

-- | Apply all updates and remove all data from local state which is
-- unnecessary for state restoration.
tidyLocalState :: MonadIO m => AcidState st -> FilePath -> m ()
tidyLocalState st path =
    liftIO (createCheckpoint st) >> createAndDiscardArchive st path

-- | ExtendedState is like usual AcidState, but also stores
-- information about FilePath (unless it's in memory).
data ExtendedState st
    = ESLocal (AcidState st)
              FilePath
    | ESMemory (AcidState st)

-- | Convert ExtendedState to AcidState.
extendedStateToAcid :: ExtendedState st -> AcidState st
extendedStateToAcid (ESLocal s _) = s
extendedStateToAcid (ESMemory s)  = s

-- | Like query', but works on ExtendedState.
queryExtended
    :: (EventState event ~ st, QueryEvent event, MonadIO m)
    => ExtendedState st -> event -> m (EventResult event)
queryExtended st = query' (extendedStateToAcid st)

-- | Like update', but works on ExtendedState.
updateExtended
    :: (EventState event ~ st, UpdateEvent event, MonadIO m)
    => ExtendedState st -> event -> m (EventResult event)
updateExtended st = update' (extendedStateToAcid st)

-- | Like openLocalStateFrom, but returns ExtendedState and operates
-- in MonadIO.
openLocalExtendedState
    :: (IsAcidic st, Typeable st, MonadIO m)
    => FilePath -> st -> m (ExtendedState st)
openLocalExtendedState fp st =
    liftIO $ flip ESLocal fp <$> openLocalStateFrom fp st

-- | Like openMemoryState, but returns ExtendedState and operates in
-- MonadIO.
openMemoryExtendedState
    :: (IsAcidic st, Typeable st, MonadIO m)
    => st -> m (ExtendedState st)
openMemoryExtendedState st = liftIO $ ESMemory <$> openMemoryState st

-- | Like closeAcidState, but operates on ExtendedState and in
-- MonadIO.
closeExtendedState :: MonadIO m => ExtendedState st -> m ()
closeExtendedState = liftIO . closeAcidState . extendedStateToAcid

-- | Like tidyLocalState, but operates on ExtendedState.
tidyExtendedState :: MonadIO m => ExtendedState st -> m ()
tidyExtendedState (ESLocal st fp) = tidyLocalState st fp
tidyExtendedState (ESMemory _)    = return ()
