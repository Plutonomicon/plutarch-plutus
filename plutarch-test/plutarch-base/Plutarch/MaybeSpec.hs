module Plutarch.MaybeSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Bool (PEq ((#==)))
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Test

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
