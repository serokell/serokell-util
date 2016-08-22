{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

-- | Collect statistics about acid-state database.

module Serokell.AcidState.Statistics
       ( StoragePart

       , estimateMemoryUsage
       ) where

import           Control.Lens               (Getter, (^.))
import qualified Data.ByteString            as BS (length)
import           Data.SafeCopy              (SafeCopy, safePut)
import           Data.Serialize.Put         (runPut)
import           Data.Text                  (Text)

import           Serokell.Data.Memory.Units (MemoryUnit, fromBytes)

type PartName = Text

data StoragePart storage = forall part. SafeCopy part =>
                                        StoragePart
    { spName   :: PartName
    , spGetter :: Getter storage part
    }

estimateMemoryUsage
    :: MemoryUnit unit
    => [StoragePart s] -> s -> [(PartName, unit)]
estimateMemoryUsage parts storage = map processPart parts
  where
    processPart StoragePart{..} =
        ( spName
        , fromBytes . toInteger . BS.length . runPut . safePut . (storage ^.) $
          spGetter)
