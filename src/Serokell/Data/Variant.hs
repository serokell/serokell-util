-- | Variant is a data type with dynamic structure. It's designed to
-- be very generic, but also efficient. You can choose whatever data
-- layout you want. This module re-exports modules relevant to this
-- type.

module Serokell.Data.Variant
       (
         module Export
       ) where

import Serokell.Data.Variant.Class as Export
import Serokell.Data.Variant.Helpers as Export
import Serokell.Data.Variant.Serialization ()
import Serokell.Data.Variant.Variant as Export
