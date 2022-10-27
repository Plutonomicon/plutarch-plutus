module Plutarch.StringSpec (spec) where

import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

{-# HLINT ignore spec "Monoid law, left identity" #-}
spec :: Spec
spec = do
  describe "str" $ do
    pgoldenSpec $ do
      "eq" @| "foo" #== pconstant @PString "foo" @-> passert
      "semigroup" @\ do
        let s1 = pconstant @PString "foo"
            s2 = pconstant @PString "bar"
        "laws" @\ do
          "id.1" @| (mempty <> s1) #== s1 @-> passert
          "id.2" @| s1 #== (mempty <> s1) @-> passert
        "concat" @| s1 <> s2 #== pconstant @PString "foobar" @-> passert
        "mempty" @| mempty #== pconstant @PString "" @-> passert
