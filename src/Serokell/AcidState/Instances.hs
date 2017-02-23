{-# LANGUAGE CPP #-}

-- | Helper instances for acid-state to remove boilerplate and introduce
-- redundant instances for other data types.

module Serokell.AcidState.Instances where

import           Control.Exception   (throw)
import           Control.Monad.Catch (MonadThrow (throwM))

import qualified Data.Time.Units     as Time
import           Data.Acid           (Query, Update)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM hiding (HashMap)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS hiding (HashSet)
import qualified Data.List.NonEmpty  as NE
import           Data.SafeCopy       (SafeCopy (..), contain, safeGet,
                                      safePut)

-- | Usually Queries shouldn't throw anything. This is a dirty hack.
instance MonadThrow (Query s) where
    throwM = throw

instance MonadThrow (Update s) where
    throwM = throw

instance (Eq a, Hashable a, SafeCopy a) => SafeCopy (HashSet a) where
    putCopy = contain . safePut . HS.toList
    getCopy = contain $ HS.fromList <$> safeGet

instance (Eq a, Hashable a, SafeCopy a, SafeCopy b) => SafeCopy (HashMap a b) where
    putCopy = contain . safePut . HM.toList
    getCopy = contain $ HM.fromList <$> safeGet

-- [SRK-51]: we should try to get this one into safecopy itself though it's
-- unlikely that they will choose a different implementation (if they do
-- choose a different implementation we'll have to write a migration)
--
-- update: made a PR <https://github.com/acid-state/safecopy/pull/47>;
-- remove this instance when the pull request is merged
instance SafeCopy a => SafeCopy (NE.NonEmpty a) where
    getCopy = contain $ do
        xs <- safeGet
        case NE.nonEmpty xs of
            Nothing -> fail "getCopy@NonEmpty: list can't be empty"
            Just xx -> return xx
    putCopy = contain . safePut . NE.toList
    errorTypeName _ = "NonEmpty"

#define SAFECOPY_TIME(T, TS)                     \
  instance SafeCopy T where {                    \
    getCopy = contain (fromInteger <$> safeGet); \
    putCopy = contain . safePut . toInteger;     \
    errorTypeName _ = TS }                       \

SAFECOPY_TIME(Time.Fortnight, "Fortnight")
SAFECOPY_TIME(Time.Week, "Week")
SAFECOPY_TIME(Time.Day, "Day")
SAFECOPY_TIME(Time.Hour, "Hour")
SAFECOPY_TIME(Time.Minute, "Minute")
SAFECOPY_TIME(Time.Second, "Second")
SAFECOPY_TIME(Time.Millisecond, "Millisecond")
SAFECOPY_TIME(Time.Microsecond, "Microsecond")
SAFECOPY_TIME(Time.Nanosecond, "Nanosecond")
SAFECOPY_TIME(Time.Picosecond, "Picosecond")
SAFECOPY_TIME(Time.Femtosecond, "Femtosecond")
SAFECOPY_TIME(Time.Attosecond, "Attosecond")

#undef SAFECOPY_TIME
