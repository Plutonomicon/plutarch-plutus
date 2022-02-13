{-# LANGUAGE CPP #-}
module Plutarch.PlutusTypeSpec (spec) where 

import Test.Syd

import Plutarch 
import Plutarch.Prelude 

import Plutarch.Test

spec :: Spec
spec = do 
  describe "plutustype" $ do
    describe "example" $ do
      describe "A encoded as 0" $ do
        let p :: Term s PInteger
            p = 0
        it "works" (pcon A #@?= p)
      describe "B encoded as 1" $ do
        let p :: Term s PInteger
            p = 1
        it "works" $ pcon B #@?= p
      describe "swap A == B" $ do
        it "works" $ swap (pcon A) #@?= pcon B
      describe "swap B == A" $ do
        it "works" $ swap (pcon B) #@?= pcon A
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
