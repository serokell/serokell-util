{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Test.Serokell.Data.Memory.UnitsSpec
       ( spec
       ) where

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (..), Gen, oneof, (===))

import           Serokell.Arbitrary         ()
import qualified Serokell.Data.Memory.Units as S

{-
data SomeMemoryUnit =
    forall u. (Arbitrary u, Eq u, Show u, S.MemoryUnit u) =>
    Mem u

instance Show SomeMemoryUnit where
    show (Mem u) = show u

instance Arbitrary SomeMemoryUnit where
    arbitrary =
        oneof
            [ Mem <$> (arbitrary :: Gen S.Byte)
            , Mem <$> (arbitrary :: Gen S.Kilobyte)
            , Mem <$> (arbitrary :: Gen S.Megabyte)
            , Mem <$> (arbitrary :: Gen S.Gigabyte)
            , Mem <$> (arbitrary :: Gen S.Terabyte)
            ]
-}

spec :: Spec
spec = describe "Memory" $ do
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
