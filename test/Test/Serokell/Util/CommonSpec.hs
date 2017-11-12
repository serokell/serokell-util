{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns              #-}

module Test.Serokell.Util.CommonSpec
       ( spec
       ) where

import Data.Foldable (toList)
import Data.List (genericIndex, genericLength, intersect)
import Data.Vector (Vector)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, NonEmptyList (..), oneof)
import Test.QuickCheck.Instances ()

import qualified Serokell.Util.Common as C

spec :: Spec
spec =
    describe "Common" $ do
        describe "enumerate" $
            prop description_enumerateCheck enumerateCheckIndexes
        describe "indexModulo" $
            prop description_indexModulo indexModuloCorrectIndex
        describe "indexedSubList" $
            prop description_indexedSubListNeg indexedSublistWhenNegative
  where
    description_enumerateCheck =
      "enumerates a structure in sequence, " ++
      "starting from index 0"
    description_indexModulo =
      "returns the element of the the list with " ++
      "given index modulo length of the list"
    description_indexedSubListNeg =
      "negative indices, given that the lower " ++
      "bound is lesser than the upper bound, result in valid indexation"

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
    in null values || indexList == [0 :: Word .. indexNum-1]

indexModuloCorrectIndex
    :: NonEmptyList Int -> Int -> Bool
indexModuloCorrectIndex (getNonEmpty -> list) ind =
    let len = genericLength list
        atModuloIndex = list `genericIndex` (ind `mod` len)
    in atModuloIndex == C.indexModulo list ind

indexedSublistWhenNegative
    :: (Int, Int) -> [Int] -> Bool
indexedSublistWhenNegative (lo, hi) list
    | hi <= lo = indexList lo hi == []
    | hi <= 0 = indexList lo hi == []
    | len -1 < lo = indexList lo hi == []
    | otherwise = testIndexes lo hi == indexList lo hi
  where
    len = length list
    indexList l h = map fst $ C.indexedSubList (l, h) list
    testIndexes l h = intersect [l .. h - 1] [0 .. len - 1]
