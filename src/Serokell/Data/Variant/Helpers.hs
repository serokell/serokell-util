-- | Helper functions for convenient work with Variant. Feel free to
-- add more if you need.

module Serokell.Data.Variant.Helpers
       ( none
       , varMap
       ) where

import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM hiding (HashMap)

import Serokell.Data.Variant.Variant (Variant (..))

-- | Shorter alias for VarNone.
none :: Variant
none = VarNone

-- | Create VarMap from Foldable containing pairs of Variants.
-- TODO: maybe use better approach like `a := b` to create KeyValuePair.
varMap :: Foldable t => t (Variant, Variant) -> Variant
varMap = VarMap . HM.fromList . toList
