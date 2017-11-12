module Test.Serokell.Util.VerifySpec
       ( spec
       ) where

import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===), (==>))
import Test.QuickCheck.Instances ()

import Data.Semigroup ((<>))
import qualified Data.Text as T (Text)

import Serokell.Arbitrary ()
import qualified Serokell.Util.Verify as V

spec :: Spec
spec =
    describe "Verify" $ do
        describe "isVerSuccess" $
            prop description_isVerSuccess isVerSuccessWorks
        describe "isVerSuccess" $
            prop description_isVerFailure isVerFailureWorks
        describe "Monoid VerificationRes" $
            prop description_verificationResIsMonoid verificationResIsMonoid
        describe "verifyGeneric" $ do
            it "verifyGeneric of an empty string always succeeds"
                (V.verifyGeneric [] `shouldBe` V.VerSuccess)
            prop description_verifyGeneric verifyGenericWorks
            prop description_verifyGenericMessageOrder verifyGenericMessageOrder
  where
    description_isVerSuccess =
        "A successful verification should yield True, and an unsuccessful one should\
        \ yield False"
    description_isVerFailure =
        "An unsuccessful verification should yield True, and an unsuccessful one should\
        \ yield False"
    description_verificationResIsMonoid =
       "The set of verification results with the concatenation operation defined in the\
       \ Serokell.Util.Verify module forms a monoid"
    description_verifyGeneric =
        "A list of predicates with a single failure yields a failed verification, and a\
        \ list of predicates with no failures yields a successful verification"
    description_verifyGenericMessageOrder =
        "Error messages in a predicate list are taken from left to right"

isVerSuccessWorks :: V.VerificationRes -> Expectation
isVerSuccessWorks v = case v of
    V.VerSuccess   -> V.isVerSuccess v `shouldBe` True
    V.VerFailure _ -> V.isVerSuccess v `shouldBe` False

-- | The property
-- `isVerFailure = not . isVerSuccess`
-- should hold for all v in VerificationRes
isVerFailureWorks :: V.VerificationRes -> Property
isVerFailureWorks v = (not . V.isVerSuccess $ v) === V.isVerFailure v

verificationResIsMonoid
    :: V.VerificationRes
    -> V.VerificationRes
    -> V.VerificationRes
    -> Bool
verificationResIsMonoid v1 v2 v3 =
    let isAssociative =
            let assoc1 = (v1 <> v2) <> v3
                assoc2 = v1 <> (v2 <> v3)
            in assoc1 == assoc2
        hasIdentity =
            let id1 = mempty <> v1
                id2 = v1 <> mempty
            in (v1 == id1) && (v1 == id2)
    in isAssociative && hasIdentity

-- | The function `verifyGeneric` must have the following properties
-- (any (not . fst) l :: [(Bool, a)]) iff (isVerFailure . verifyGeneric) l
-- (all (identity . fst) l :: [(Bool, a)]) iff (isVerSuccess . verifyGeneric) l

verifyGenericWorks :: [(Bool, T.Text)] -> Bool
verifyGenericWorks l =
    let someFailure  = any (not . fst) l
        allSuccess   = not someFailure
    in (someFailure == (V.isVerFailure . V.verifyGeneric $ l)) &&
       (allSuccess  == (V.isVerSuccess . V.verifyGeneric $ l))

-- | This property checks that the error messages in a list of predicates are collected
-- from left to right.

verifyGenericMessageOrder :: [(Bool, T.Text)] -> Property
verifyGenericMessageOrder l =
    let someFailure  = any (not . fst) l
        msgList = foldr (\(b, err) accList ->
                            if not b then err : accList
                                  else accList) [] l
    in someFailure ==>
       ((V.VerFailure msgList) == V.verifyGeneric l)
