-- | Some useful functions to work with Data.Acid

module Serokell.Util.AcidState
       ( createAndDiscardArchive
       , exceptStateToUpdate
       , exceptStateToUpdateGeneric
       , readerToQuery
       , stateToUpdate
       ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad.Reader       (Reader, asks, runReader)
import           Control.Monad.State        (State, runState, state)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Acid                  (AcidState, Query, Update,
                                             createArchive)
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
-- discard it.
createAndDiscardArchive :: AcidState st -> FilePath -> IO ()
createAndDiscardArchive st path =
    createArchive st >> removeDirectoryRecursive (path </> "Archive")
