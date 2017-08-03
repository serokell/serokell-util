{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Serokell.Util.I18N
       ( Translations
       , fromYaml
       , replaceTranslations
       , YamlMapKey
       , ToReplaceToken(..)
       ) where

import qualified Data.Aeson             as AT
import           Data.ByteString        as BS
import qualified Data.Map.Strict        as M
import           Data.Text              as T
import           Data.Yaml              (decodeEither)
import           Formatting             (build, sformat, (%))
import           GHC.Generics           (Generic, Rep)
import           Serokell.Aeson.Options (defaultOptions)


-- It's better to get rid of aeson-extra depricated things here, but
-- aeson-1.0.0.0 structure of FromJSONKey requires fromJSONKeyList
-- which is unobvious to implement.
class (Eq a, Ord a, AT.FromJSONKey a, AT.FromJSON a) => YamlMapKey a

#if MIN_VERSION_aeson(1,0,0)
instance (Eq a,
          Ord a,
          Generic a,
          AT.GFromJSON AT.Zero (Rep a),
          AT.FromJSONKey a,
          AT.FromJSON a) =>
#else
instance (Eq a, Ord a, Generic a, AT.GFromJSON (Rep a)) =>
#endif
         YamlMapKey a

type Translations lang token = M.Map lang (M.Map token T.Text)

fromYaml :: (YamlMapKey lang, YamlMapKey token)
         => BS.ByteString
         -> Translations lang token
fromYaml yamlStr =
    either
        (error .
         T.unpack . sformat ("Error during translation YAML parsing " % build))
        id $
    decodeEither yamlStr

class (Ord a, Eq a) => ToReplaceToken a where
    toReplaceToken :: a -> T.Text

#if MIN_VERSION_aeson(1,0,0)
instance (Ord a, Eq a, Generic a, AT.GToJSON AT.Zero (Rep a)) => ToReplaceToken a where
    toReplaceToken = (\(AT.String s) -> s) . AT.genericToJSON defaultOptions
#else
instance (Ord a, Eq a, Generic a, AT.GToJSON (Rep a)) => ToReplaceToken a where
    toReplaceToken = (\(AT.String s) -> s) . AT.genericToJSON defaultOptions
#endif

replaceTranslations
    :: (ToReplaceToken token, Ord lang)
    => Translations lang token -> lang -> T.Text -> Maybe T.Text
replaceTranslations translations lang text =
    M.foldrWithKey
        (\token ->
              T.replace $ "{{" `T.append` toReplaceToken token `T.append` "}}")
        text <$>
    lang `M.lookup` translations
