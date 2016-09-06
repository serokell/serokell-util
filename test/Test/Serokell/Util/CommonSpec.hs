{-# LANGUAGE ViewPatterns #-}

module Test.Serokell.Util.CommonSpec
       ( spec
       ) where

import           Data.Foldable         (toList)
import           Data.List             (genericIndex, genericLength)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonEmptyList (..))

import qualified Serokell.Util.Common  as C

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

enumerateCheckIndexes
    :: (Traversable t, Show a, Show (t a))
    => t a -> Bool
enumerateCheckIndexes values =
    let indexedVals = C.enumerate values
        indexList   = toList $ fmap fst indexedVals
        indexNum    = genericLength indexList
    in indexList == [0 .. indexNum-1]

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
