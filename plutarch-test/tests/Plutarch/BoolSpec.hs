module Plutarch.BoolSpec (spec) where

import Plutarch.Bool (pand, por)
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "bool" . pgoldenSpec $ do
    "pnot" @\ do
      "lam" @| pnot
      "app" @| pnot # pcon PTrue @-> passertNot
    "pand" @\ do
      "tf" @| pcon PTrue #&& pcon PFalse @-> passertNot
      "ft" @| pcon PFalse #&& pcon PTrue @-> passertNot
      "tt" @| pcon PTrue #&& pcon PTrue @-> passert
      "ff" @| pcon PFalse #&& pcon PFalse @-> passertNot
      "laziness" @\ do
        "pand" @| pand # pcon PFalse # pdelay perror @-> \p ->
          passert $ pnot # pforce p
        "op"
          @| pcon PFalse
          #&& perror
          @-> \p ->
            passert $ pnot # p
        "pand.perror" @\ do
          -- FIXME
          -- "false" @| pand # pcon PFalse # perror @-> pfails
          -- "true" @| pand # pcon PTrue # perror @-> pfails
          "op" @| pcon PTrue #&& perror @-> pfails
    "por" @\ do
      "tf" @| pcon PTrue #|| pcon PFalse @-> passert
      "ft" @| pcon PFalse #|| pcon PTrue @-> passert
      "tt" @| pcon PTrue #|| pcon PTrue @-> passert
      "ff" @| pcon PFalse #|| pcon PFalse @-> passertNot
      "laziness" @\ do
        "por" @| por # pcon PTrue # pdelay perror @-> \p ->
          passert (pforce p)
        "op"
          @| pcon PTrue
          #|| perror
          @-> \p ->
            passert p
        "pand.perror" @\ do
          -- FIXME
          -- "false" @| por # pcon PFalse # perror @-> pfails
          -- "true" @| por # pcon PTrue # perror @-> pfails
          "op.true" @| pcon PTrue #|| perror @-> psucceeds
          "op.false" @| pcon PFalse #|| perror @-> pfails
