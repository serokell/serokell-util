{-# LANGUAGE ScopedTypeVariables #-}

module Test.Serokell.Data.Memory.UnitsSpec
       ( spec
       ) where

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck.Arbitrary  (Arbitrary)

import qualified Serokell.Data.Memory.Units as S

{-newtype SomeMemoryUnit = MemUnit
    { getUnit :: forall unit . (Show unit, Eq unit, S.MemoryUnit unit) => SomeMemoryUnit unit
    } deriving (Show, Eq)-}

deriving instance Arbitrary S.Byte

spec :: Spec
spec = describe "Memory" $ do
           describe "Identity Properties" $ do
               prop "Byte" $
                   \(a :: S.Byte) -> a === bytesMid a
               prop "Kilobyte" $
                   \(a :: S.Kilboyte) -> a === bytesMid a


bytesMid
    :: S.MemoryUnit unit
    => unit -> unit
bytesMid = S.convertUnit
