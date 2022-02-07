module Plutarch.BoolSpec (tests) where

import Test.Tasty.HUnit

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

-- TODO: Finish boolean tests
tests :: TestTreeM ()
tests = do
  ttGroup "bool" $ do
    ttGroup "pnot" $ do
      golden "pnot" pnot
      golden "pnot.app" (pnot #$ pcon PTrue)
      tt $ testCase "pnot.true" $ (pnot #$ pcon PTrue) #@?= pcon PFalse
      tt $ testCase "pnot.false" $ (pnot #$ pcon PFalse) #@?= pcon PTrue
    ttGroup "lazy" $ do
      ttGroup "pand" $ do
        goldens
          "lazy.pand"
          [ ("tf", pcon PTrue #&& pcon PFalse)
          , ("ft", pcon PFalse #&& pcon PTrue)
          , ("tt", pcon PTrue #&& pcon PTrue)
          , ("ff", pcon PFalse #&& pcon PFalse)
          ]
        tt $ testCase "tf" $ (pcon PTrue #&& pcon PFalse) #@?= pcon PFalse
        tt $ testCase "ft" $ (pcon PFalse #&& pcon PTrue) #@?= pcon PFalse
        tt $ testCase "tt" $ (pcon PTrue #&& pcon PTrue) #@?= pcon PTrue
        tt $ testCase "ff" $ (pcon PFalse #&& pcon PFalse) #@?= pcon PFalse
