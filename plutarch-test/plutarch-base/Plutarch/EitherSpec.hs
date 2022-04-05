module Plutarch.EitherSpec (spec) where

import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "either" . pgoldenSpec $ do
    "eq" @\ do
      "true" @\ do
        "left" @| pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PLeft 42) @-> passert
        "right" @| pcon @(PEither PInteger PInteger) (PRight 42) #== pcon (PRight 42) @-> passert
      "false" @\ do
        "left-right" @| pcon @(PEither PInteger PInteger) (PLeft 42) #== pcon (PRight 42) @-> passertNot
        "left-left" @| pcon @(PEither PInteger PInteger) (PLeft 24) #== pcon (PLeft 42) @-> passertNot
        "right-right" @| pcon @(PEither PInteger PInteger) (PRight 24) #== pcon (PRight 42) @-> passertNot
