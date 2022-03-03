module Plutarch.ExtraSpec (spec) where

import Test.Syd (Spec, describe, it)

spec :: Spec
spec =
  describe "Hello Plutarch Extra" $
    it "says Hello Plutarch Extra" $ putStrLn "Hello Plutarch Extra"
