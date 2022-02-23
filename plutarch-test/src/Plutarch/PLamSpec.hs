module Plutarch.PLamSpec (spec) where

import Test.Syd

import Data.ByteString (ByteString)
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "plam" . pgoldenSpec $ do
    "id" @| plam (\x -> x)
    "flip.const" @| plam (\_ y -> y)
    "plet" @| plam (\x _ -> plet x $ \_ -> perror)
    "primitives" @\ do
      "bool" @\ do
        "true" @| plam $ \_ -> pconstant True
      "int" @\ do
        "0" @| plam $ \_ -> (0 :: Term _ PInteger)
        "1" @| plam $ \_ -> (1 :: Term _ PInteger)
        "512" @| plam $ \_ -> (512 :: Term _ PInteger)
        "1048576" @| plam $ \_ -> (1048576 :: Term _ PInteger)
      "bytestring" @\ do
        "1" @| plam $ \_ -> pconstant ("1" :: ByteString)
        "1111111" @| plam $ \_ -> pconstant ("1111111" :: ByteString)
      "unit" @\ do
        "list" @| plam $ \_ -> pconstant ([()] :: [()])
        "()" @| plam $ \_ -> pconstant ()
      "id" @| plam $ \x -> x
      "fun" @\ do
        "lam+" @| plam $ \_ -> (plam (+) :: Term _ (PInteger :--> PInteger :--> PInteger))
        "+" @| (plam (+) :: Term _ (PInteger :--> PInteger :--> PInteger))
