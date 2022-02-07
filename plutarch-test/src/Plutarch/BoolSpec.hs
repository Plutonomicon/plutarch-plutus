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
      golden "" pnot
      golden "app" (pnot #$ pcon PTrue)
      tt $ testCase "true" $ (pnot #$ pcon PTrue) #@?= pcon PFalse
      tt $ testCase "false" $ (pnot #$ pcon PFalse) #@?= pcon PTrue
    ttGroup "lazy" $ do
      ttGroup "pand" $ do
        goldens
          ""
          [ ("tf", pcon PTrue #&& pcon PFalse)
          , ("ft", pcon PFalse #&& pcon PTrue)
          , ("tt", pcon PTrue #&& pcon PTrue)
          , ("ff", pcon PFalse #&& pcon PFalse)
          ]
        tt $ testCase "tf" $ (pcon PTrue #&& pcon PFalse) #@?= pcon PFalse
        tt $ testCase "ft" $ (pcon PFalse #&& pcon PTrue) #@?= pcon PFalse
        tt $ testCase "tt" $ (pcon PTrue #&& pcon PTrue) #@?= pcon PTrue
        tt $ testCase "ff" $ (pcon PFalse #&& pcon PFalse) #@?= pcon PFalse
