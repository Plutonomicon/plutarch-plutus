module Plutarch.RationalSpec (spec) where

import Test.Syd

import Plutarch.Prelude
import Plutarch.Rational (pproperFraction, ptruncate)
import Plutarch.Test

spec :: Spec
spec = do
  describe "rational" $ do
    describe "literal" $ do
      let p = 0.5 :: Term s PRational
      golden PrintTerm p
    describe "ops" $ do
      let p1 = (1 / 2 + 1 / 2) :: Term s PRational
          p2 = 1 / 2 - 1 / 3 :: Term s PRational
          p3 = (1 - 3 / 2) * (2 - 5 / 2) :: Term s PRational
      goldens
        All
        [ ("+", p1)
        , ("-", p2)
        , ("*", p3)
        ]
      it "+" $ passert $ p1 #== 1
      it "-" $ passert $ p2 #== 1 / 6
      it "*" $ passert $ p3 #== 1 / 4
      it "harmonic-sum" $
        passert $ 1 / 2 + 1 / 3 + 1 / 4 + 1 / 5 #== (77 / 60 :: Term s PRational)
      it "multi-product" $
        passert $ 1 / 2 * 2 / 3 * 3 / 4 * 4 / 5 * 5 / 6 #== (1 / 6 :: Term s PRational)
    describe "compare" $ do
      let p1 = 2 / 9 #< (3 / 10 :: Term s PRational)
      goldens All [("<", p1)]
      it "<" $ passert p1
    describe "round" $ do
      -- NOTE: These will eventually be replaced by property tests.
      it "5/3" $ passert $ pround # (5 / 3) #== 2
      it "4/3" $ passert $ pround # (4 / 3) #== 1
      it "-5/2" $ passert $ pround # (-5 / 2) #== -2
      it "-1/4" $ passert $ pround # (-1 / 4) #== 0
    describe "truncate" $ do
      it "5/4" $ passert $ ptruncate # (5 / 4) #== 1
      it "7/4" $ passert $ ptruncate # (7 / 4) #== 1
      it "1/4" $ passert $ ptruncate # (1 / 4) #== 0
      it "-7/4" $ passert $ ptruncate # (-7 / 4) #== -1
    describe "properFraction" $ do
      it "-1/2" $
        passert $
          pmatch (pproperFraction # (-1 / 2)) $ \(PPair x y) ->
            x #== 0 #&& y #== (-1 / 2)
      it "-3/2" $
        passert $
          pmatch (pproperFraction # (-3 / 2)) $ \(PPair x y) ->
            x #== -1 #&& y #== (-1 / 2)
      it "-4/3" $
        passert $
          pmatch (pproperFraction # (-4 / 3)) $ \(PPair x y) ->
            x #== -1 #&& y #== (-1 / 3)
    describe "data" $ do
      describe "id" $ do
        it "0.5" $ passert $ (0.5 :: Term s PRational) #== pfromData (pdata 0.5)
        it "2" $ passert $ (2 :: Term s PRational) #== pfromData (pdata 2)
        it "11/3" $ passert $ (11 / 3 :: Term s PRational) #== pfromData (pdata $ 11 / 3)
