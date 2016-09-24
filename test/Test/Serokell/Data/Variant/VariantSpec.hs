{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Serokell.Data.Variant.VariantSpec
       ( spec
       ) where

import qualified Data.Aeson            as A (decode, encode)
import qualified Data.Binary           as B (decode, encode)
import qualified Data.HashMap.Lazy     as HM (elems, fromList, keys)
import           Data.MessagePack      (fromObject, toObject)
import qualified Data.SafeCopy         as SC (safeGet, safePut)
import           Data.Scientific       (floatingOrInteger, fromFloatDigits)
import qualified Data.Serialize        as C (decode, encode, runGet, runPut)
import           Data.Text             (unpack)
import qualified Data.Vector           as V (map)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((===))

import           Serokell.Arbitrary    (VariantNoBytes (..),
                                        VariantOnlyBytes (..))
import qualified Serokell.Data.Variant as S
import qualified Serokell.Util.Base64  as S
import           Serokell.Util.Text    (show')

spec :: Spec
spec = describe "Variant" $ do
           describe "Identity Properties" $ do
               describe "JSON" $ do
                   prop "Variant (No VarBytes)" $
                       \(getVariant -> a) -> (jsonFixer a) === jsonMid a
                   prop "Variant (Only VarBytes)" $
                       \(getVarBytes -> a) -> a === (bytesFun $ jsonMid a)
               prop "MessagePack" $
                   \(a :: S.Variant) -> (msgPkFixer a) === msgPkMid a
               prop "Binary" $
                   \(a :: S.Variant) -> a === binMid a
               prop "SafeCopy" $
                   \(a :: S.Variant) -> a === safeCopyMid a
               prop "Serialize" $
                   \(a :: S.Variant) -> a === cerealMid a

jsonFixer :: S.Variant -> S.Variant
jsonFixer (S.VarMap m) = let ks = map toStr $ HM.keys m
                             vs = map jsonFixer $ HM.elems m
                             m' = HM.fromList $ zip ks vs
                         in S.VarMap m'
jsonFixer (S.VarList l) = S.VarList $ V.map jsonFixer l
jsonFixer v@(S.VarInt i) =
    if i < 0 then v
             else (S.VarUInt $ fromIntegral i)
jsonFixer (S.VarFloat f) =
    case floatingOrInteger $ fromFloatDigits f of
        Left float -> S.VarFloat float
        Right int -> if int < 0 then S.VarInt int
                                else S.VarUInt $ fromIntegral int
jsonFixer v = v

toStr :: S.Variant -> S.Variant
toStr var = stringVar var
  where
    stringVar :: S.Variant -> S.Variant
    stringVar = S.VarString . show'

jsonMid :: S.Variant -> S.Variant
jsonMid = maybe err id . A.decode . A.encode
   where
     err = error "[VariantSpec] Failed JSON decoding"

bytesFun :: S.Variant -> S.Variant
bytesFun (S.VarString s) = S.VarBytes right
  where
     right = either (error . unpack) id $ S.decode s
bytesFun _ = error "[bytesFun:] called with Variant that was not VarBytes"

msgPkFixer :: S.Variant -> S.Variant
msgPkFixer (S.VarMap m) = let ks = map msgPkFixer $ HM.keys m
                              vs = map msgPkFixer $ HM.elems m
                              m' = HM.fromList $ zip ks vs
                          in S.VarMap m'
msgPkFixer (S.VarList l) = S.VarList $ V.map msgPkFixer l
msgPkFixer v@(S.VarInt i) =
    if i < 0 then v
             else (S.VarUInt $ fromIntegral i)
msgPkFixer v@(S.VarUInt i) =
    if i >= 0 then v
             else (S.VarInt $ fromIntegral i)
msgPkFixer v = v

msgPkMid :: S.Variant -> S.Variant
msgPkMid = maybe err id . fromObject . toObject
  where
    err = error "[VariantSpec] Failed MessagePack decoding"

binMid :: S.Variant -> S.Variant
binMid = B.decode . B.encode

cerealMid :: S.Variant -> S.Variant
cerealMid = either error id . C.decode . C.encode

safeCopyMid :: S.Variant -> S.Variant
safeCopyMid = either error id . C.runGet SC.safeGet . C.runPut . SC.safePut
