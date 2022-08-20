module Plutarch.Extra.ByteStringSpec (spec) where

import Data.Char (ord)

import Plutarch.Extra.ByteString (pallBS, pisHexDigit)

import Plutarch.Prelude

import Plutarch.ListSpec (integerList)

import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = describe "extra.bytestring" . pgoldenSpec $ do
  "allandhexdigit" @\ do
    "allas" @| pallBS # plam (#== pconstant (toInteger $ ord 'a')) # pconstant "aaaaaaaaaaaaa" @-> passert
    "not all as" @| pallBS # plam (#== pconstant (toInteger $ ord 'a')) # pconstant "aaaaaabaaaaa" @-> passertNot
    "allhex" @| pallBS # pisHexDigit # pconstant "5a7c18eae8778d15344f" @-> passert
    "notallhex" @| pallBS # pisHexDigit # pconstant "5a7c18eae8778g15344f" @-> passertNot
    "pisHexDigit" @| pall @PList # pisHexDigit # integerList (toInteger . ord <$> "1234567890abcdef") @-> passert
    "pisNoneHexDigit" @| pany @PList # pisHexDigit # integerList (toInteger . ord <$> "ghikjklmnopqrstuvwxyz !@#$%^&*()[]{}`~") @-> passertNot
