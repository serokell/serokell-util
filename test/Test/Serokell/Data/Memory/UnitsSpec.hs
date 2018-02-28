{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Test.Serokell.Data.Memory.UnitsSpec
       ( spec
       ) where

import Universum

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Serokell.Arbitrary ()

import qualified Serokell.Data.Memory.Units as S

spec :: Spec
spec = describe "Unit conversion" $ do
           describe "Identity Properties" $ do
               prop "Byte" $
                   \(a :: S.Byte) -> a === bytesMid a
               prop "Kilobyte" $
                   \(a :: S.Kilobyte) -> a === bytesMid a
               prop "Megabyte" $
                   \(a :: S.Megabyte) -> a === bytesMid a
               prop "Gigabyte" $
                   \(a :: S.Gigabyte) -> a === bytesMid a
               prop "Terabyte" $
                   \(a :: S.Terabyte) -> a === bytesMid a

bytesMid :: forall u. S.MemoryUnit u => u -> u
bytesMid = S.convertUnit
