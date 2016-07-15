-- | Options used to derive FromJSON/ToJSON instance. These options
-- generally comply to our style regarding names. Of course sometimes
-- they don't fit one's needs, so treat them as just sensible
-- defaults.

module Serokell.Aeson.Options
       ( defaultOptions
       , leaveTagOptions
       , defaultOptionsPS
       ) where

import qualified Data.Aeson.TH as A
import           Data.Char     (isLower, isPunctuation, isUpper, toLower)
import           Data.List     (findIndex)

headToLower :: String -> String
headToLower [] = undefined
headToLower (x:xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
    maybe t (flip drop t . decrementSafe) $ findIndex isLower t
  where
    decrementSafe 0 = 0
    decrementSafe i = i - 1

-- | These options do the following transformations:
-- 1. Names of field
-- records are assumed to be camelCased, `camel` part is removed,
-- `Cased` part is converted to `cased`. So `camelCased` becomes
-- `cased`. Also all punctuation symbols are dropped before doing it.
-- 2. Constructors are assumed to start with some capitalized prefix
-- (which finished right before the last capital letter). This prefix
-- is dropped and then the first letter is lowercased.
defaultOptions :: A.Options
defaultOptions =
    A.defaultOptions
    { A.fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
    , A.constructorTagModifier = headToLower . stripConstructorPrefix
    , A.sumEncoding = A.ObjectWithSingleField
    }

-- | These options are the same as `defaultOptions`, but they don't
-- modify constructor tags.
leaveTagOptions :: A.Options
leaveTagOptions = defaultOptions { A.constructorTagModifier = id }

-- | Options used for communication with PureScript by default.
defaultOptionsPS :: A.Options
defaultOptionsPS = A.defaultOptions
