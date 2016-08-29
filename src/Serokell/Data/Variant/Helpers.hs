-- | Helper functions for convenient work with Variant. Feel free to
-- add more if you need.

module Serokell.Data.Variant.Helpers
       ( none
       ) where

import           Serokell.Data.Variant.Variant (Variant (..))

none :: Variant
none = VarNone
