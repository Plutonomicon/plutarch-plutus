module Plutarch.Extra.MaybeSpec (spec) where

import Plutarch.Extra.Maybe
import Plutarch.Prelude

import Data.Maybe (fromJust, isJust)
import Hedgehog (Property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (propertyTest)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Test
import Plutarch.Test.Property
import Plutarch.Test.Property.Gen
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

spec :: Spec
spec = modifyMaxSuccess (const 10_000) $ do
  describe "extra.maybeutils" $ do
    describe "properties" $ do
      it "plutarch level fromJust behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pfromJust
      it "plutarch level isJust behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pisJust
      it "plutarch level Just behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pjust
    pgoldenSpec $ do
      "fromJust" @\ do
        "fromJust_Just" @| (5 #== (pfromJust #$ pjust # marshal (5 :: Integer))) @-> passert
        "fromJust_Nothing" @| pfromJust # pnothing @-> pfails
      "isJust" @\ do
        "isJust_Just" @| (pisJust #$ pjust # marshal (5 :: Integer)) @-> passert
        "isJust_Nothing" @| (pnot #$ pisJust # pnothing) @-> passert
      "Just" @\ do
        "Just_0" @| (pcon (PJust 0) #== pjust # marshal (0 :: Integer)) @-> passert
      "fromDJust" @\ do
        "fromDJust_Just" @| (5 #== (pfromDJust #$ pdjust # marshal (5 :: Integer))) @-> passert
        "fromDJust_Nothing" @| pfromDJust # (pdnothing :: Term s (PMaybeData PInteger)) @-> pfails
      "isDJust" @\ do
        "isDJust_Just" @| (pisDJust #$ pdjust # marshal (5 :: Integer)) @-> passert
        "isDJust_Nothing" @| (pnot #$ pisDJust # (pdnothing :: Term s (PMaybeData PInteger))) @-> passert

prop_pfromJust :: Property
prop_pfromJust = do
  prop_haskEquiv
    @OnPEq
    @PartialFun
    (fromJust :: Maybe Integer -> Integer)
    pfromJust
    (Gen.maybe genInteger :* Nil)

prop_pisJust :: Property
prop_pisJust = do
  prop_haskEquiv
    @OnPEq
    @TotalFun
    (isJust :: Maybe Integer -> Bool)
    pisJust
    (Gen.maybe genInteger :* Nil)

prop_pjust :: Property
prop_pjust = do
  prop_haskEquiv
    @OnPEq
    @TotalFun
    (Just :: Integer -> Maybe Integer)
    pjust
    (genInteger :* Nil)
