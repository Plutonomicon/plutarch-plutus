module Plutarch.UnitSpec (spec) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = runTrailSpec $ do
  describe "unit" . pgoldenSpec $ do
    "pcon" @| pcon PUnit
    "pmatch" @| pmatch (pcon PUnit) (\case PUnit -> pcon PTrue) @-> passert
    "compare" @\ do
      "==" @| pcon PUnit #== pcon PUnit @-> passert
      "<" @| pcon PUnit #< pcon PUnit @-> passertNot
      "<=" @| pcon PUnit #<= pcon PUnit @-> passert
