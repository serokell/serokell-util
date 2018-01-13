{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

-- | General-purpose utility functions

module Serokell.Util.Verify
       ( VerificationRes (..)

       -- * Helpers
       , isVerFailure
       , isVerSuccess
       , verResToMonadError

       -- * Verification
       , verifyGeneric

       -- * Prety printing
       , formatAllErrors
       , formatFirstError
       , verResFullF
       , verResSingleF
       ) where

import Universum

import Control.Monad.Except (MonadError, throwError)

import Serokell.Util.Text (listBuilder)

import qualified Data.Text.Lazy.Builder as B

data VerificationRes
    = VerSuccess
    | VerFailure !(NonEmpty Text)
    deriving (Eq, Show)

isVerSuccess :: VerificationRes -> Bool
isVerSuccess VerSuccess = True
isVerSuccess _          = False

isVerFailure :: VerificationRes -> Bool
isVerFailure (VerFailure _) = True
isVerFailure _              = False

instance Semigroup VerificationRes where
    VerSuccess    <> a             = a
    a             <> VerSuccess    = a
    VerFailure xs <> VerFailure ys = VerFailure $ xs <> ys

instance Monoid VerificationRes where
    mempty = VerSuccess
    mappend = (<>)

-- | This function takes list of (predicate, message) pairs and checks
-- each predicate.  If predicate is False it's considered an error.
-- If there is at least one error this function returns VerFailure,
-- otherwise VerSuccess is returned.  It's useful to verify some data
-- before using it.
-- Example usage: `verifyGeneric [(checkA, "A is bad"), (checkB, "B is bad")]`
verifyGeneric :: [(Bool, Text)] -> VerificationRes
verifyGeneric errors = case messages of
    []     -> VerSuccess
    (x:xs) -> VerFailure $ x :| xs
  where
    messages = map snd . filter (not . fst) $ errors

----------------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------------

-- | Format VerificationRes in a pretty way using all errors messages
-- for VerFailure.
verResFullF :: VerificationRes -> B.Builder
verResFullF VerSuccess          = "success"
verResFullF (VerFailure errors) = buildVerResImpl errors

-- | Format VerificationRes in a pretty way using only first message
-- for VerFailure.
verResSingleF :: VerificationRes -> B.Builder
verResSingleF VerSuccess          = "success"
verResSingleF (VerFailure errors) = buildVerResImpl $ one $ head errors

buildVerResImpl :: NonEmpty Text -> B.Builder
buildVerResImpl errors =
    "failure: " <> listBuilder @Text @Text @Text "[" "; " "]" errors

-- These two functions can have more general type.

-- | Pretty printer for errors from VerFailure, all errors are printed.
formatAllErrors :: NonEmpty Text -> B.Builder
formatAllErrors = verResFullF . VerFailure

-- | Pretty printer for errors from VerFailure, only first error is printed.
formatFirstError :: NonEmpty Text -> B.Builder
formatFirstError = verResSingleF . VerFailure

----------------------------------------------------------------------------
-- Conversion to MonadError (including Either)
----------------------------------------------------------------------------

verResToMonadError
    :: MonadError e m
    => (NonEmpty Text -> e) -> VerificationRes -> m ()
verResToMonadError _ VerSuccess          = pure ()
verResToMonadError f (VerFailure errors) = throwError $ f errors
