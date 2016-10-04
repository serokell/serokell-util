-- | General-purpose utility functions

module Serokell.Util.Verify
       ( VerificationRes (..)
       , verifyGeneric
       ) where

import qualified Data.Text as T

data VerificationRes
    = Verified
    | NotVerified ![T.Text]
    deriving (Show)

instance Monoid VerificationRes where
    mempty = Verified
    mappend Verified a                        = a
    mappend a Verified                        = a
    mappend (NotVerified xs) (NotVerified ys) = NotVerified $ xs ++ ys

-- | This function takes list of (predicate, message) pairs and checks
-- each predicate.  If predicate is False it's considered an error.
-- If there is at least one error this function returns NotVerified,
-- otherwise Verified is returned.  It's useful to verify some data
-- before using it.
-- Example usage: `verifyGeneric [(checkA, "A is bad"), (checkB, "B is bad")]`
verifyGeneric :: [(Bool, T.Text)] -> VerificationRes
verifyGeneric errors
    | null messages = Verified
    | otherwise = NotVerified messages
  where
    messages = map snd . filter (not . fst) $ errors
