module Plutarch.BoolSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Bool (PBool (PFalse, PTrue), pand, pnot, por, (#&&), (#||))
import Plutarch.Test

spec :: Spec
spec = do
  describe "bool" $ do
    describe "pnot" $ do
      pgoldenSpec $ do
        "lam" @> pnot
        "app" @> pnot #$ pcon PTrue
      it "true" $ (pnot #$ pcon PTrue) #@?= pcon PFalse
      it "false" $ (pnot #$ pcon PFalse) #@?= pcon PTrue
    describe "pand" $ do
      pgoldenSpec $ do
        "tf" @> pcon PTrue #&& pcon PFalse @-> pshouldBe (pcon PFalse)
        "ft" @> pcon PFalse #&& pcon PTrue @-> pshouldBe (pcon PFalse)
        "tt" @> pcon PTrue #&& pcon PTrue @-> passert
        "ff" @> pcon PFalse #&& pcon PFalse @-> pshouldBe (pcon PFalse)
      describe "laziness" $ do
        pgoldenSpec $ do
          "pand" @> pand # pcon PFalse # pdelay perror @-> \p ->
            passert $ pnot # pforce p
          "op" @> pcon PFalse #&& perror @-> \p ->
            passert $ pnot # p
        it "pand.perror" $ do
          pfails $ pand # pcon PFalse # perror
          pfails $ pand # pcon PTrue # perror
          pfails $ pcon PTrue #&& perror
    describe "por" $ do
      pgoldenSpec $ do
        "tf" @> pcon PTrue #|| pcon PFalse @-> passert
        "ft" @> pcon PFalse #|| pcon PTrue @-> passert
        "tt" @> pcon PTrue #|| pcon PTrue @-> passert
        "ff" @> pcon PFalse #|| pcon PFalse @-> passertNot
      describe "laziness" $ do
        pgoldenSpec $ do
          "por" @> por # pcon PTrue # pdelay perror @-> \p ->
            passert (pforce p)
          "op" @> pcon PTrue #|| perror @-> \p ->
            passert p
        it "pand.perror" $ do
          pfails $ por # pcon PFalse # perror
          pfails $ por # pcon PTrue # perror
          passert $ pcon PTrue #|| perror
          pfails $ pcon PFalse #|| perror
