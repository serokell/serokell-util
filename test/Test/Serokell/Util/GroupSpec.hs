module Test.Serokell.Util.GroupSpec
       ( spec
       ) where

import Universum

import Data.List.Extra (nubOn)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import Serokell.Util.Group

spec :: Spec
spec =
    describe "Group" $ do
        describe "'groupBy' doesn't throw elements away" $ do
            prop "with id"   groupByIdWorks
            prop "with even" groupByEvenWorks
            prop "with mod"  groupByModWorks
            prop "with fst"  groupByFstWorks
        describe "'groupMapBy' behaves like 'nubOn'" $ do
            prop "with id"   groupMapByIdWorks
            prop "with even" groupMapByEvenWorks
            prop "with mod"  groupMapByModWorks
            prop "with fst"  groupMapByFstWorks


----------------------------------------------------------------------------
-- GroupBy
----------------------------------------------------------------------------
groupByWorks :: (Ord a, Eq b, Hashable b) => (a -> b) -> NonEmpty a -> Bool
groupByWorks f l = sort (mconcat (fmap toList (elems (groupBy f l)))) == sort (toList l)

groupByIdWorks :: NonEmpty Int -> Bool
groupByIdWorks = groupByWorks id

groupByEvenWorks :: NonEmpty Int -> Bool
groupByEvenWorks = groupByWorks even

groupByModWorks :: NonEmpty Int -> Bool
groupByModWorks = groupByWorks (`mod` 100)

groupByFstWorks :: NonEmpty (Int, Int) -> Bool
groupByFstWorks = groupByWorks fst

----------------------------------------------------------------------------
-- GroupMapBy
----------------------------------------------------------------------------

groupMapByWorks :: (Ord a, Eq b, Hashable b) => (a -> b) -> [a] -> Bool
groupMapByWorks f l = sort (elems (groupMapBy f l)) == sort (nubOn f l)

groupMapByIdWorks :: [Int] -> Bool
groupMapByIdWorks = groupMapByWorks id

groupMapByEvenWorks :: [Int] -> Bool
groupMapByEvenWorks = groupMapByWorks even

groupMapByModWorks :: [Int] -> Bool
groupMapByModWorks = groupMapByWorks (`mod` 100)

groupMapByFstWorks :: [(Int, Int)] -> Bool
groupMapByFstWorks = groupMapByWorks fst
