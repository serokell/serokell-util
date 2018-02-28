{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}

module Test.Serokell.Util.TextSpec
       ( spec
       ) where

import Universum

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))
import Test.QuickCheck.Instances ()

import qualified Serokell.Util.Text as S

spec :: Spec
spec =
    describe "Text Show/Read" $ do
        describe "Indentity Properties" $ do
            describe "readDecimal" $ do
                prop "Int" $
                    \(a :: Int) -> (Right a) === showReadIntegral a
                prop "Integer" $
                    \(a :: Integer) -> (Right a) === showReadIntegral a
                prop "Word" $
                    \(a :: Word) -> (Right a) === showReadIntegral a
                prop "Int64" $
                    \(a :: Int64) -> (Right a) === showReadIntegral a
                prop "Word64" $
                    \(a :: Word64) -> (Right a) === showReadIntegral a

showReadIntegral :: (Show a, Integral a) => a -> Either String a
showReadIntegral = S.readDecimal . show @Text
