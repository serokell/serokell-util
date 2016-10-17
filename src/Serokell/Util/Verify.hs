-- | General-purpose utility functions

module Serokell.Util.Verify
       ( VerificationRes (..)
       , isVerFailure
       , isVerSuccess

       , verifyGeneric
       ) where

import           Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import qualified Data.Text      as T

data VerificationRes
    = VerSuccess
    | VerFailure ![T.Text]
    deriving (Show)

isVerSuccess :: VerificationRes -> Bool
isVerSuccess VerSuccess = True
isVerSuccess _          = False

isVerFailure :: VerificationRes -> Bool
isVerFailure (VerFailure _) = True
isVerFailure _              = False

instance Semigroup VerificationRes where
    VerSuccess <> a = a
    VerFailure xs <> a =
        VerFailure $
        xs ++
        case a of
            VerSuccess    -> []
            VerFailure ys -> ys

instance Monoid VerificationRes where
    mempty = VerSuccess
    mappend = (Semigroup.<>)

-- | This function takes list of (predicate, message) pairs and checks
-- each predicate.  If predicate is False it's considered an error.
-- If there is at least one error this function returns VerFailure,
-- otherwise VerSuccess is returned.  It's useful to verify some data
-- before using it.
-- Example usage: `verifyGeneric [(checkA, "A is bad"), (checkB, "B is bad")]`
verifyGeneric :: [(Bool, T.Text)] -> VerificationRes
verifyGeneric errors
    | null messages = VerSuccess
    | otherwise = VerFailure messages
  where
    messages = map snd . filter (not . fst) $ errors
