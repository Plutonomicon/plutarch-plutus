module Plutarch.BoolSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

-- TODO: Finish boolean tests
spec :: Spec
spec = do
  describe "bool" $ do
    describe "pnot" $ do
      goldens
        [ ("lam", popaque pnot)
        , ("app", popaque $ pnot #$ pcon PTrue)
        ]
      it "true" $
        (pnot #$ pcon PTrue) #@?= pcon PFalse
      it "false" $
        (pnot #$ pcon PFalse) #@?= pcon PTrue
    describe "lazy" $ do
      describe "pand" $ do
        goldens
          [ ("tf", popaque $ pcon PTrue #&& pcon PFalse)
          , ("ft", popaque $ pcon PFalse #&& pcon PTrue)
          , ("tt", popaque $ pcon PTrue #&& pcon PTrue)
          , ("ff", popaque $ pcon PFalse #&& pcon PFalse)
          ]
        it "tf" $ (pcon PTrue #&& pcon PFalse) #@?= pcon PFalse
        it "ft" $ (pcon PFalse #&& pcon PTrue) #@?= pcon PFalse
        it "tt" $ (pcon PTrue #&& pcon PTrue) #@?= pcon PTrue
        it "ff" $ (pcon PFalse #&& pcon PFalse) #@?= pcon PFalse
