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
            prop "with id"   groupByIdProp
            prop "with even" groupByEvenProp
            prop "with mod"  groupByModProp
            prop "with fst"  groupByFstProp
        describe "'groupMapBy' behaves like 'nubOn'" $ do
            prop "with id"   groupMapByIdProp
            prop "with even" groupMapByEvenProp
            prop "with mod"  groupMapByModProp
            prop "with fst"  groupMapByFstProp


----------------------------------------------------------------------------
-- GroupBy
----------------------------------------------------------------------------

-- | Check that 'groupBy' doesn't throw elements away
groupByProp :: (Ord a, Eq b, Hashable b) => (a -> b) -> NonEmpty a -> Bool
groupByProp f l = foldMap toList (elems (groupBy f l)) ~=~ toList l

groupByIdProp, groupByEvenProp, groupByModProp :: NonEmpty Int -> Bool
groupByIdProp   = groupByProp id
groupByEvenProp = groupByProp even
groupByModProp  = groupByProp (`mod` 100)

groupByFstProp :: NonEmpty (Int, Int) -> Bool
groupByFstProp = groupByProp fst

----------------------------------------------------------------------------
-- GroupMapBy
----------------------------------------------------------------------------

-- | Checks that 'groupMapBy' behaves like 'nubOn'.
groupMapByProp :: (Ord a, Eq b, Hashable b) => (a -> b) -> [a] -> Bool
groupMapByProp f l = elems (groupMapBy f l) ~=~ nubOn f l

groupMapByIdProp, groupMapByEvenProp, groupMapByModProp :: [Int] -> Bool
groupMapByIdProp   = groupMapByProp id
groupMapByEvenProp = groupMapByProp even
groupMapByModProp  = groupMapByProp (`mod` 100)

groupMapByFstProp :: [(Int, Int)] -> Bool
groupMapByFstProp = groupMapByProp fst

----------------------------------------------------------------------------

-- | Sorts lists before comparing them.
infix 4 ~=~
(~=~) :: (Ord a) => [a] -> [a] -> Bool
l1 ~=~ l2 = sort l1 == sort l2
