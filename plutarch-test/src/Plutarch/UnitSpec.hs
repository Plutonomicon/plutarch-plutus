module Plutarch.UnitSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "unit" $ do
    describe "pcon" $ do
      golden All $ pcon PUnit
    describe "pmatch" $ do
      let p = pmatch (pcon PUnit) (\case PUnit -> pcon PTrue)
      golden All p
      it "works" $ passert p
    describe "compare" $ do
      let pEq = pcon PUnit #== pcon PUnit
          pLt = pcon PUnit #< pcon PUnit
          pLe = pcon PUnit #<= pcon PUnit
      goldens
        All
        [ ("==", pEq)
        , ("<", pLt)
        , ("<=", pLe)
        ]
      it "==" $ passert pEq
      it "<" $ passert $ pnot # pLt
      it "<=" $ passert pLe
