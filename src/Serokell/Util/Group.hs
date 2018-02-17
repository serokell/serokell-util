{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module introduces functions for grouping.

module Serokell.Util.Group
       ( groupBy
       , groupMapBy
       ) where

import Universum

import Data.List.NonEmpty ((<|))

import qualified Data.HashMap.Strict as HM

{- | Groups elements using results of the given function as keys.

>>> groupBy even [1,2,3,4,5,6]
fromList [(False,5 :| [3,1]),(True,6 :| [4,2])]

-}
groupBy :: forall t a b . (Container t, Element t ~ a, Eq b, Hashable b)
        => (a -> b) -> t -> HashMap b (NonEmpty a)
groupBy f = foldl' hmGroup mempty
  where
    hmGroup :: a -> HashMap b (NonEmpty a) -> HashMap b (NonEmpty a)
    hmGroup x =
        let val :: Maybe (NonEmpty a) -> NonEmpty a
            val Nothing   = one x
            val (Just xs) = x <| xs
        in HM.alter (Just . val) (f x)

{- | Similar to 'groupBy' but keeps only one element as value.

>>> groupMapBy even [1,2,3,4,5,6]
fromList [(False,1),(True,2)]

-}
groupMapBy :: forall t a b . (Container t, Element t ~ a, Eq b, Hashable b)
           => (a -> b) -> t -> HashMap b a
groupMapBy f = foldl' hmGroup mempty
  where
    hmGroup :: a -> HashMap b a -> HashMap b a
    hmGroup val hm = let key = f val in
        case HM.lookup key hm of
            Nothing -> HM.insert key val hm
            Just _  -> hm
