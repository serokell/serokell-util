{-# LANGUAGE TypeFamilies #-}

-- | Helper functions for convenient work with Variant. Feel free to
-- add more if you need.

module Serokell.Data.Variant.Helpers
       ( none
       , varMap
       ) where

import Universum

import Serokell.Data.Variant.Variant (Variant (..))

import qualified Data.HashMap.Strict as HM

-- | Shorter alias for VarNone.
none :: Variant
none = VarNone

-- | Create VarMap from Foldable containing pairs of Variants.
-- TODO: maybe use better approach like `a := b` to create KeyValuePair.
varMap :: (Container t, Element t ~ (Variant, Variant)) => t -> Variant
varMap = VarMap . HM.fromList . toList
