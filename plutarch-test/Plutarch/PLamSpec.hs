module Plutarch.PLamSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Test

spec :: Spec
spec = do
  describe "plam" $ do
    describe "id" $ do
      golden $ plam (\x -> x)
    describe "flip.const" $ do
      golden $ plam (\_ y -> y)
    describe "plet" $ do
      golden $ plam (\x _ -> plet x $ \_ -> perror)
