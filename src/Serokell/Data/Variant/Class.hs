-- | Type class for values which can be converted to/from Variant.
-- Also contains some instances.

module Serokell.Data.Variant.Class
       ( ToVariant (toVariant)
       , FromVariant (fromVariant)
       ) where

import           Control.Monad.Catch           (MonadThrow)
import           Formatting                    (build, sformat, (%))

import           Serokell.Data.Variant.Variant (Variant (..))
import           Serokell.Util.Exceptions      (throwText)

class ToVariant v where
    toVariant :: v -> Variant

class FromVariant v where
    -- TODO: it won't be used now, so I didn't think much about this type.
    -- Maybe it's not the best idea to use MonadThrow here.
    fromVariant :: MonadThrow m => Variant -> m v

instance ToVariant Bool where
    toVariant = VarBool

instance ToVariant Double where
    toVariant = VarFloat

instance FromVariant Double where
    fromVariant (VarFloat v) = pure v
    fromVariant v = throwText $ sformat ("value is not Double: " % build) v
