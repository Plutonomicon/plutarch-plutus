module Plutarch.Extra.EqOrdSpec (
  spec,
) where

import Control.Monad (forM_, liftM2)
import Hedgehog (Gen)
import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (Marshal (marshal), haskPlutEquiv, viaData)

import Plutarch.Prelude

import Plutarch.Extra.API.V1 ()

import Plutarch.Test

import Data.String (IsString (fromString))
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "EqOrdSpec" $ do
  describe "MaybeInt" $ testEqOrd $ EGen.maybeOf EGen.integer
  describe "PairInt" $ testEqOrd $ EGen.pairOfBoth EGen.integer
  describe "goldens" $ do
    describe "MaybeInt" $ goldensEqOrd [Nothing, Just 1, Just 2 :: Maybe Integer]
    describe "PairInt" $ goldensEqOrd [(1, 2), (2, 1), (1, 1), (2, 2) :: (Integer, Integer)]

-- TODO refactor goldens once https://github.com/Plutonomicon/plutarch/pull/323 is merged
-- goldenEqOrd should become top level (or at least get a type signature)
-- goldensEqOrd should return PlutarchGoldens (by droping the `pgoldenSpec $`)
-- the goldens block should then be rewriten to use pgoldenSpec only once at top level

goldensEqOrd :: forall a p. (Marshal a p, PEq p, POrd p) => [a] -> Spec
goldensEqOrd xs = pgoldenSpec $
  forM_ (zip [1 :: Int ..] (liftM2 (,) xs xs)) $
    \(n, (x, y)) -> fromString ("exa" ++ show n) @\ goldenEqOrd x y
  where
    -- this can't be type signed because the types are not exposed :(
    -- https://github.com/Plutonomicon/plutarch/pull/323
    goldenEqOrd l (r :: a) = do
      "==" @| marshal l #== marshal r
      "<=" @| marshal l #<= marshal r
      "<" @| marshal l #< marshal r

testEqOrd :: forall h p. (Show h, Ord h, PEq p, POrd p, Marshal h p) => Gen h -> Spec
testEqOrd g = do
  eqSpec g
  lteSpec g
  ltSpec g

eqSpec :: forall h p. (Show h, Eq h, PEq p, Marshal h p) => Gen h -> Spec
eqSpec g =
  it "==" $
    haskPlutEquiv
      viaData
      ((==) @h)
      (plam (#==))
      (g, g)

lteSpec :: forall h p. (Show h, Ord h, POrd p, Marshal h p) => Gen h -> Spec
lteSpec g =
  it "<=" $
    haskPlutEquiv
      viaData
      ((<=) @h)
      (plam (#<=))
      (g, g)

ltSpec :: forall h p. (Show h, Ord h, POrd p, Marshal h p) => Gen h -> Spec
ltSpec g =
  it "<" $
    haskPlutEquiv
      viaData
      ((<) @h)
      (plam (#<))
      (g, g)
