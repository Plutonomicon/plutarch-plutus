module Plutarch.MaybeSpec (spec) where

import Plutarch
import Plutarch.Bool (PEq ((#==)))
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PJust, PNothing), pfromJust)
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "maybe" . pgoldenSpec $ do
    "eq" @\ do
      "true" @\ do
        "nothing" @| pcon @(PMaybe PInteger) PNothing #== pcon PNothing @-> passert
        "just" @| pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42) @-> passert
      "false" @\ do
        "nothing-just" @| pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42) @-> passertNot
        "just-just" @| pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42) @-> passertNot
    "pfromJust" @\ do
      "nothing" @| pfromJust # pcon PNothing @-> pfails
      "just" @| pfromJust # pcon (PJust 42) #== (42 :: Term _ PInteger) @-> passert
