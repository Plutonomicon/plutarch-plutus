module Plutarch.BoolSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Bool (PBool (PFalse, PTrue), pand, pnot, por, (#&&), (#||))
import Plutarch.Test

spec :: Spec
spec = do
  describe "bool" $ do
    describe "pnot" $ do
      goldens
        All
        [ ("lam", popaque pnot)
        , ("app", popaque $ pnot #$ pcon PTrue)
        ]
      it "true" $ (pnot #$ pcon PTrue) #@?= pcon PFalse
      it "false" $ (pnot #$ pcon PFalse) #@?= pcon PTrue
    describe "pand" $ do
      goldens
        All
        [ ("tf", pcon PTrue #&& pcon PFalse)
        , ("ft", pcon PFalse #&& pcon PTrue)
        , ("tt", pcon PTrue #&& pcon PTrue)
        , ("ff", pcon PFalse #&& pcon PFalse)
        ]
      it "tf" $ (pcon PTrue #&& pcon PFalse) #@?= pcon PFalse
      it "ft" $ (pcon PFalse #&& pcon PTrue) #@?= pcon PFalse
      it "tt" $ (pcon PTrue #&& pcon PTrue) #@?= pcon PTrue
      it "ff" $ (pcon PFalse #&& pcon PFalse) #@?= pcon PFalse
      describe "laziness" $ do
        let p1 = pand # pcon PFalse # pdelay perror
            p2 = pcon PFalse #&& perror
        goldens All [("pand", popaque p1), ("op", popaque p2)]
        it "pand" $ passert $ pnot # pforce p1
        it "op" $ passert $ pnot # p2
        it "pand.perror" $ do
          pfails $ pand # pcon PFalse # perror
          pfails $ pand # pcon PTrue # perror
          pfails $ pcon PTrue #&& perror
    describe "por" $ do
      goldens
        All
        [ ("tf", pcon PTrue #|| pcon PFalse)
        , ("ft", pcon PFalse #|| pcon PTrue)
        , ("tt", pcon PTrue #|| pcon PTrue)
        , ("ff", pcon PFalse #|| pcon PFalse)
        ]
      it "tf" $ (pcon PTrue #|| pcon PFalse) #@?= pcon PTrue
      it "ft" $ (pcon PFalse #|| pcon PTrue) #@?= pcon PTrue
      it "tt" $ (pcon PTrue #|| pcon PTrue) #@?= pcon PTrue
      it "ff" $ (pcon PFalse #|| pcon PFalse) #@?= pcon PFalse
      describe "laziness" $ do
        let p1 = por # pcon PTrue # pdelay perror
            p2 = pcon PTrue #|| perror
        goldens All [("por", popaque p1), ("op", popaque p2)]
        it "por" $ passert $ pforce p1
        it "op" $ passert p2
        it "pand.perror" $ do
          pfails $ por # pcon PFalse # perror
          pfails $ por # pcon PTrue # perror
          passert $ pcon PTrue #|| perror
          pfails $ pcon PFalse #|| perror
