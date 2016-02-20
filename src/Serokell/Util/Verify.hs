-- | General-purpose utility functions

module Serokell.Util.Verify
       ( verifyGeneric
       ) where

import           Serokell.Util.Exceptions (throwText)

import           Control.Monad.Catch     (MonadThrow)
import qualified Data.Text               as T

-- | This function takes list of (predicate, message) pairs and checks each predicate.
-- If predicate is False it's considered an error.
-- If there is at least one error this function throws error message, otherwise () is returned
-- It's useful to verify some data before using it.
-- Example usage: `verifyGeneric [(checkA, "A is bad"), (checkB, "B is bad")]`
verifyGeneric :: MonadThrow m
              => [(Bool, T.Text)] -> m ()
verifyGeneric errors
  | not $ null messages = throwText $ T.intercalate " " messages
  where messages = map snd . filter (not . fst) $ errors
verifyGeneric _
  | otherwise = return ()
