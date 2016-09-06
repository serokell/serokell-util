{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns              #-}

module Test.Serokell.Util.CommonSpec
       ( spec
       ) where

import           Data.Foldable             (toList)
import           Data.List                 (genericIndex, genericLength)
import           Data.Vector               (Vector)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Arbitrary (..), Gen,
                                            NonEmptyList (..), oneof)
import           Test.QuickCheck.Instances ()

import qualified Serokell.Util.Common      as C

spec :: Spec
spec =
    describe "Common" $ do
        describe "enumerate" $
            prop description_enumerateCheck enumerateCheckIndexes
        describe "indexModulo" $
            prop description_indexModulo indexModuloCorrectIndex
  where
    description_enumerateCheck = "enumerates a structure in sequence, " ++
      "starting from index 0"
    description_indexModulo = "returns the element of the the list with " ++
      "given index modulo length of the list"

-- TODO: consider using `SomeValue` to generate different types of
-- values.  I don't know how to add more than one quantifier to type
-- definition. Also consider using GADTs to achieve it.
type Value = Int

data SomeTraversable =
    forall t. (Traversable t, Show (t Value), Arbitrary (t Value)) =>
              SomeTraversable (t Value)

instance Show SomeTraversable where
    show (SomeTraversable t) = show t

instance Arbitrary SomeTraversable where
    arbitrary =
        oneof
            [ SomeTraversable <$> (arbitrary :: Gen [Value])
            , SomeTraversable <$> (arbitrary :: Gen (Vector Value))
            ]

enumerateCheckIndexes :: SomeTraversable -> Bool
enumerateCheckIndexes (SomeTraversable values) =
    let indexedVals = C.enumerate values
        indexList   = toList $ fmap fst indexedVals
        indexNum    = genericLength indexList
    in indexList == [0 :: Word .. indexNum-1]

indexModuloCorrectIndex
    :: NonEmptyList Int -> Int -> Bool
indexModuloCorrectIndex (getNonEmpty -> list) ind =
    let len = genericLength list
        atModuloIndex = list `genericIndex` (ind `mod` len)
    in atModuloIndex == C.indexModulo list ind

indexedSublistCheckIndexes
    :: (Int, Int) -> [Int] -> Bool
indexedSublistCheckIndexes (lo, hi) list
    | hi <= lo = indexList == []
    | otherwise = [lo .. hi - 1] == indexList

  where
    indexList = map fst $ C.indexedSubList (lo, hi) list
