module Plutarch.Extra.IsDataSpec (
  spec,
) where

import Hedgehog (Gen, Property)

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Gen (Marshal (marshal), NotLambda, leftInverse, viaPEq)

import Plutarch.Prelude

import Plutarch.Test

import Data.Map (Map)
import qualified Data.Map as M

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "isData" $ do
  it "Maybe" $ isDataTest $ EGen.maybeOf EGen.integer
  it "map" $ isDataTest EGen.defaultMap
  pgoldenSpec $ do
    "maybe" @\ do
      "Just_1" @| pdata $ marshal (Just 1 :: Maybe Integer)
      "Nothing" @| pdata $ marshal (Nothing :: Maybe Integer)
    "map" @\ do
      "ex1" @| pdata $ marshal (M.fromList [(1, 1 / 2), (2, 2 / 3)] :: Map Integer Rational)
      "ex2" @| pdata $ marshal (M.empty :: Map Integer Rational)

isDataTest :: forall a a'. (Show a, Marshal a a', PEq a', PIsData a', NotLambda a) => Gen a -> Property
isDataTest =
  leftInverse
    viaPEq
    (plam pfromData)
    (plam pdata)
