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
      -- TODO: Can we use `goldens` here on non-equivalent terms?
      golden pnot
      golden' "app" (pnot #$ pcon PTrue)
      it "true" $
        (pnot #$ pcon PTrue) #@?= pcon PFalse
      it "false" $
        (pnot #$ pcon PFalse) #@?= pcon PTrue
    describe "lazy" $ do
      describe "pand" $ do
        goldens
          [ ("tf", pcon PTrue #&& pcon PFalse)
          , ("ft", pcon PFalse #&& pcon PTrue)
          , ("tt", pcon PTrue #&& pcon PTrue)
          , ("ff", pcon PFalse #&& pcon PFalse)
          ]
        it "tf" $ (pcon PTrue #&& pcon PFalse) #@?= pcon PFalse
        it "ft" $ (pcon PFalse #&& pcon PTrue) #@?= pcon PFalse
        it "tt" $ (pcon PTrue #&& pcon PTrue) #@?= pcon PTrue
        it "ff" $ (pcon PFalse #&& pcon PFalse) #@?= pcon PFalse
