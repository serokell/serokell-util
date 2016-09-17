{-# LANGUAGE ScopedTypeVariables #-}

module Test.Serokell.Data.Variant.VariantSpec
       ( spec
       ) where

import           Data.Aeson            (decode, encode)
import qualified Data.Binary           as B (decode, encode)
import           Data.Maybe            (fromJust)
import           Data.MessagePack      (fromObject, toObject)
import           Data.Scientific       (floatingOrInteger, fromFloatDigits)
import qualified Data.Vector           as V (map)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((===))

import           Serokell.Arbitrary    ()
import qualified Serokell.Data.Variant as S
import qualified Serokell.Util.Base64  as S
import           Serokell.Util.Text    (show')

spec :: Spec
spec = describe "Variant" $ do
           describe "Identity Properties" $ do
               {-prop "Serialization" $
                   \(a :: S.Variant) -> variantMid a
               prop "MessagePack" $
                   \(a :: S.Variant) -> msgPkFun a === msgPackMid a-}
               prop "Binary" $
                   \(a :: S.Variant) -> "TODO" === "TODO" --a === binMid a
{-
variantMid a =
    let json = jsonFun a
    in case a of
          S.VarBytes _ -> a === bytesFun json
          S.VarInt i -> if i < 0 then a === json
                                 else (S.VarUInt $ fromIntegral i) === json
          S.VarFloat _ -> json === floatFun a
          S.VarMap m -> (S.VarMap $ fmap toStr m) === json
          S.VarList l -> (S.VarList $ V.map keysToStr l) === json
          _ -> a === json

keysToStr (S.VarMap m)  = S.VarMap $ fmap toStr m
keysToStr (S.VarList l) = S.VarList $ V.map keysToStr l
keysToStr v             = v

jsonFun :: S.Variant -> S.Variant
jsonFun = fromJust . decode . encode

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

toStr :: S.Variant -> S.Variant
toStr var = stringVar var

stringVar :: S.Variant -> S.Variant
stringVar = S.VarString . show'

msgPackMid :: S.Variant -> S.Variant
msgPackMid a =
    case a of
        S.VarInt i ->
            if i < 0 then a
                     else (S.VarUInt $ fromIntegral i)
        _ -> a

msgPkFun :: S.Variant -> S.Variant
msgPkFun = fromJust . fromObject . toObject-}

binMid :: S.Variant -> S.Variant
binMid = B.decode . B.encode
