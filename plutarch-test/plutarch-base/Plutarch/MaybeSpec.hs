module Plutarch.MaybeSpec (spec) where

import Plutarch
import Plutarch.Bool (PEq ((#==)))
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Test
import qualified Plutarch.Test.TrailSpecMonad as TS
import Test.Hspec

spec :: Spec
spec = TS.runTrailSpec $ do
  TS.describe "maybe" . pgoldenSpec $ do
    "eq" @\ do
      "true" @\ do
        "nothing" @| pcon @(PMaybe PInteger) PNothing #== pcon PNothing @-> passert
        "just" @| pcon @(PMaybe PInteger) (PJust 42) #== pcon (PJust 42) @-> passert
      "false" @\ do
        "nothing-just" @| pcon @(PMaybe PInteger) PNothing #== pcon (PJust 42) @-> passertNot
        "just-just" @| pcon @(PMaybe PInteger) (PJust 24) #== pcon (PJust 42) @-> passertNot
