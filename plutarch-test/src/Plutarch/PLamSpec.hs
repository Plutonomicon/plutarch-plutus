module Plutarch.PLamSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Test

spec :: Spec
spec = do
  describe "plam" . pgoldenSpec $ do
    "id" @| plam (\x -> x)
    "flip.const" @| plam (\_ y -> y)
    "plet" @| plam (\x _ -> plet x $ \_ -> perror)
