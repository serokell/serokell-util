-- | Helper instances for Acid State to remove boilerplate and introduce
-- redundant instances for other data types.

module Serokell.AcidState.Instances where

import           Control.Exception   (throw)
import           Control.Monad.Catch (MonadThrow (throwM))

import           Data.Acid           (Update)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM hiding (HashMap)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS hiding (HashSet)
import           Data.SafeCopy       (SafeCopy (getCopy, putCopy), contain,
                                      safeGet, safePut)

instance MonadThrow (Update s) where
    throwM = throw

instance (Eq a, Hashable a, SafeCopy a) => SafeCopy (HashSet a) where
    putCopy = contain . safePut . HS.toList
    getCopy = contain $ HS.fromList <$> safeGet

instance (Eq a, Hashable a, SafeCopy a, SafeCopy b) => SafeCopy (HashMap a b) where
    putCopy = contain . safePut . HM.toList
    getCopy = contain $ HM.fromList <$> safeGet
