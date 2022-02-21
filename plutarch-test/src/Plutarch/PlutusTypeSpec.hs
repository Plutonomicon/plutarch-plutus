{-# LANGUAGE CPP #-}

module Plutarch.PlutusTypeSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Prelude

import Plutarch.Test

spec :: Spec
spec = do
  describe "plutustype" $ do
    describe "example" . pgoldenSpec $ do
      "A-as-0" @| pcon A @== pconstant @PInteger 0
      "B-as-1" @| pcon B @== pconstant @PInteger 1
      "swap" @\ do
        "A" @| swap (pcon A) @== pcon B
        "B" @| swap (pcon B) @== pcon A
    describe "instances-sanity" $ do
      plutarchDevFlagDescribe $ do
        it "PBuiltinList" $ do
          pmatchTargetEval $ pconstant [1 :: Integer, 2, 3, 4]

-- | Make sure the target of 'pmatch' is only evaluated once.
pmatchTargetEval :: PlutusType p => ClosedTerm p -> Expectation
pmatchTargetEval target =
  pmatch (ptrace (pconstant tag) target) (\x -> plet (pcon x) $ \_ -> pconstant ())
    `ptraces` replicate 1 tag
  where
    tag = "evaluating"

{- TODO:
    - move over the testcase with pmatchTargetEval
    - add more sanity checks
describe "sanity checks" $ do
  describe "PBuiltinList" $ do
    let p :: Term s (PBuiltinList PInteger)
        p = pconstant [1,2,3,4]
    it "works" $
 -}

{- |
  A Sum type, which can be encoded as an Enum
-}
data AB (s :: S) = A | B

{- |
  AB is encoded as an Enum, using values of PInteger
  internally.
-}
instance PlutusType AB where
  type PInner AB _ = PInteger

  pcon' A = 0
  pcon' B = 1

  pmatch' x f =
    pif (x #== 0) (f A) (f B)

{- |
  Instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type.
-}
swap :: Term s AB -> Term s AB
swap x = pmatch x $ \case
  A -> pcon B
  B -> pcon A
