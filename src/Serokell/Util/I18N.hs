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

import           Data.Aeson.Extra.Map   (FromJSONKey (..), getMap)
import           Data.Aeson.Types       (genericParseJSON, genericToJSON)
import qualified Data.Aeson.Types       as AT
import           Data.ByteString        as BS
import qualified Data.Map.Strict        as M
import           Data.Text              as T
import           Data.Yaml              (decodeEither)
import           Formatting             (build, sformat, (%))
import           GHC.Generics           (Generic, Rep)
import           Serokell.Aeson.Options (defaultOptions)

class (Eq a, Ord a, FromJSONKey a) => YamlMapKey a

instance (Generic a, AT.GFromJSON (Rep a)) =>
         FromJSONKey a where
    parseJSONKey l = genericParseJSON defaultOptions (AT.String l)

instance (Eq a, Ord a, Generic a, AT.GFromJSON (Rep a)) =>
         YamlMapKey a

type Translations lang token = M.Map lang (M.Map token T.Text)

fromYaml :: (YamlMapKey lang, YamlMapKey token)
         => BS.ByteString
         -> Translations lang token
fromYaml yamlStr =
    toLangMap $
    either
        (error .
         T.unpack . sformat ("Error during translation YAML parsing " % build))
        id $
    decodeEither yamlStr
  where
    toLangMap = fmap getMap . getMap


class (Ord a, Eq a) => ToReplaceToken a where
  toReplaceToken :: a -> T.Text

instance (Ord a, Eq a, Generic a, AT.GToJSON (Rep a)) => ToReplaceToken a where
  toReplaceToken = (\(AT.String s) -> s) . genericToJSON defaultOptions

replaceTranslations
    :: (ToReplaceToken token, Ord lang)
    => Translations lang token -> lang -> T.Text -> Maybe T.Text
replaceTranslations translations lang text =
    M.foldrWithKey
        (\token ->
              T.replace $ "{{" `T.append` toReplaceToken token `T.append` "}}")
        text <$>
    lang `M.lookup` translations
