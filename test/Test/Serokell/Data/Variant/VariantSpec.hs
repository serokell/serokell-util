{-# LANGUAGE ScopedTypeVariables #-}

module Test.Serokell.Data.Variant.VariantSpec
       ( spec
       ) where

import           Data.Aeson             (decode, encode)
import           Data.Maybe             (fromJust)
import           Data.Scientific        (floatingOrInteger, fromFloatDigits)
import           Data.Text.Buildable    (build)
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Test.Hspec             (Spec, describe)
import           Test.Hspec.QuickCheck  (prop)
import           Test.QuickCheck        ((===))

import           Serokell.Arbitrary     ()
import qualified Serokell.Data.Variant  as S
import qualified Serokell.Util.Base64   as S

spec :: Spec
spec = describe "Variant" $ do
           describe "Identity Properties" $ do
               prop "Variant" $
                   \(a :: S.Variant) ->
                       case a of
                           S.VarBytes _ -> a === (bytesFun $ variantMid a)
                           S.VarInt i -> if i < 0 then a === variantMid a
                                                  else (S.VarUInt $ fromIntegral i) === variantMid a
                           S.VarFloat _ -> a === (floatFun $ variantMid a)
                           S.VarMap m -> let m' = fmap toStr m
                                         in m === m'
                           _            -> a === variantMid a

variantMid :: S.Variant -> S.Variant
variantMid = fromJust . decode . encode

bytesFun :: S.Variant -> S.Variant
bytesFun (S.VarString s) = S.VarBytes right
  where
     right = fromRight $ S.decode s
     fromRight (Right x) = x
     fromRight _ = error "[bytesFun:] fromRight called with Left argument!"
bytesFun _ = error "[bytesFun:] called with Variant that was not VarBytes"

floatFun :: S.Variant -> S.Variant
floatFun (S.VarFloat f) =
    case floatingOrInteger $ fromFloatDigits f of
        Left float -> S.VarFloat float
        Right int -> if int < 0 then S.VarInt int
                                else S.VarUInt $ fromIntegral int
floatFun _ = error "[floatFun:] called with Variant that was not VarFloat"

{-toStr :: S.Variant -> S.Variant
toStr var       = stringVar var

stringVar = S.VarString . toStrict . toLazyText . build
-}
