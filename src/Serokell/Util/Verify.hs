{-# LANGUAGE TypeApplications #-}

-- | General-purpose utility functions

module Serokell.Util.Verify
       ( VerificationRes (..)
       , isVerFailure
       , isVerSuccess

       -- * Prety printing
       , buildVerResFull
       , buildVerResSingle
       , verResFullF
       , verResSingleF

       , verifyGeneric
       ) where

import           Data.Semigroup         (Semigroup)
import qualified Data.Semigroup         as Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as B
import           Formatting             (Format, later)

import           Serokell.Util.Text     (listBuilder)

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

----------------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------------

-- | Format VerificationRes in a pretty way using all errors messages
-- for VerFailure.
buildVerResFull :: VerificationRes -> B.Builder
buildVerResFull VerSuccess          = buildVerResImpl Nothing
buildVerResFull (VerFailure errors) = buildVerResImpl $ Just errors

-- | Format VerificationRes in a pretty way using only first message
-- for VerFailure.
buildVerResSingle :: VerificationRes -> B.Builder
buildVerResSingle VerSuccess          = buildVerResImpl Nothing
-- [SU-1] Use NonEmpty instead of unsafe head.
buildVerResSingle (VerFailure errors) = buildVerResImpl $ Just $ [head errors]

buildVerResImpl :: Maybe [T.Text] -> B.Builder
buildVerResImpl Nothing       = "success"
buildVerResImpl (Just errors) =
    "failure: " `mappend` listBuilder @Text @Text @Text "[" "; " "]" errors

-- | Formatter based on buildVerResFull.
verResFullF :: Format r (VerificationRes -> r)
verResFullF = later buildVerResFull

-- | Formatter based on buildVerResSingle.
verResSingleF :: Format r (VerificationRes -> r)
verResSingleF = later buildVerResSingle
