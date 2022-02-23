module Plutarch.Extra.MapSpec (spec) where

import qualified Data.Map as M
import Hedgehog (Property, annotateShow, forAll, property)
import qualified Hedgehog.Gen as HGen

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (Marshal (marshal), haskPlutEquiv, leftInverse, testPEq, viaBoth, viaData, viaPEq)

import Plutarch.Prelude

import Plutarch (ClosedTerm)

import Plutarch.Extra.Integer (peven)
import Plutarch.Extra.Map (
  Map,
  adjust,
  filterWithKey,
  findWithDefault,
  mall,
  mapJoin,
  mapSplit,
  mapWithKey,
  mmap,
  msingleton,
  pinsert,
  plookup,
  unionWith,
 )

import Plutarch.Test

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

-- TODO
-- tests for
-- mapLTE
-- mapSub
-- more tests for split and join?

spec :: Spec
spec = describe "map" $ do
  let m1 = marshal $ M.fromList [(1, 0), (3, 2 / 3), (5 :: Integer, 7 :: Rational)]
      m2 = marshal $ M.fromList [(2, 1), (3, 1 / 2), (4 :: Integer, 8 :: Rational)]
      m3 = marshal $ M.fromList [(3, 1), (4, 1 / 2), (5 :: Integer, 8 :: Rational)]
  describe "insert_lookup" $ do
    it "1" insertLookup1
    it "2" insertLookup2
  it "unionWithAdd" unionWithAdd
  it "unionWithSub" unionWithSub
  it "filterWithKey" filterWithKeyTest
  it "findWithDefault" findWithDefaultTest
  it "adjust" adjustTest
  it "mapWithKey" mapWithKeyTest
  it "mall" mallTest
  it "msingleton" msingletonTest
  it "join is a left inverse of split" splitJoinTest
  it "mmap" mmapTest
  pgoldenSpec $ do
    "insert" @\ do
      "exa1" @| pinsert # 1 # (3 / 2) # m1
      "exa2" @| pinsert # 1 # (3 / 2) # m2
      "exa3" @| pinsert # 3 # (3 / 2) # m1
      "exa4" @| pinsert # 4 # (3 / 2) # m2
      "exa5" @| pinsert # 5 # (3 / 2) # m1
      "exa6" @| pinsert # 5 # (3 / 2) # m2
    "lookup" @\ do
      "exa1" @| plookup # 1 # m1
      "exa2" @| plookup # 2 # m1
      "exa3" @| plookup # 5 # m1
      "exa4" @| plookup # 7 # m1
    "unionWithAdd" @\ do
      "exa1" @| unionWith # plam (+) # m1 # m2
      "exa2" @| unionWith # plam (+) # m1 # m3
      "exa3" @| unionWith # plam (+) # m2 # m3
    "unionWithSub" @\ do
      "exa1" @| unionWith # plam (-) # m1 # m2
      "exa2" @| unionWith # plam (-) # m1 # m3
      "exa3" @| unionWith # plam (-) # m2 # m3
    "filterWithKey" @\ do
      "exa1" @| filterWithKey # plam (\x y -> peven # x #&& 0 #< y) # m1
      "exa1" @| filterWithKey # plam (\x y -> peven # x #&& 0 #< y) # m2
      "exa1" @| filterWithKey # plam (\x y -> peven # x #&& 0 #< y) # m3
    "findWithDefault" @\ do
      "exa1" @| findWithDefault # (100 :: Term s PRational) # (1 :: Term s PInteger) # m1
      "exa2" @| findWithDefault # (100 :: Term s PRational) # (2 :: Term s PInteger) # m1
      "exa3" @| findWithDefault # (100 :: Term s PRational) # (6 :: Term s PInteger) # m1
    "adjust" @\ do
      "exa1" @| adjust # plam ((* 3) :: Term s PInteger -> Term s PInteger) # m1
      "exa1" @| adjust # plam ((* 5) :: Term s PInteger -> Term s PInteger) # m2
    "mapWithKey" @\ do
      "exa1" @| mapWithKey # plam (\k v -> (pfromInteger # k) * v) # m1
      "exa1" @| mapWithKey # plam (\k v -> (pfromInteger # k) * v) # m2
    "msingleton" @\ do
      "exa1" @| msingleton # (0 :: Term s PInteger) # (1 / 2 :: Term s PRational)
      "exa2" @| msingleton # (1 :: Term s PInteger) # (3 / 2 :: Term s PRational)
    "mmap" @\ do
      "exa1" @| mmap # plam (* 2) # m1
      "exa1" @| mmap # plam (* 3) # m2
    "mall" @\ do
      "exa1" @| mall # plam (0 #<) # m1
      "exa2" @| mall # plam (0 #<) # m2

insertLookup1 :: Property
insertLookup1 = property $ do
  m <- forAll EGen.defaultMap
  x <- forAll EGen.integer
  y <- forAll EGen.rational
  annotateShow m
  annotateShow x
  annotateShow y
  let x' :: Term s PInteger
      x' = fromInteger x
      y' :: Term s PRational
      y' = fromRational y
      m1 :: Term s (Map PInteger PRational)
      m1 = marshal m
      m2 :: Term s (Map PInteger PRational)
      m2 = pinsert # x' # y' # m1
      r :: Term s (PMaybe PRational)
      r = plookup # x' # m2
  testPEq r (pcon $ PJust y')

insertLookup2 :: Property
insertLookup2 = property $ do
  x1 <- forAll EGen.integer
  x2 <- forAll $ HGen.filter (/= x1) EGen.integer
  m <- forAll EGen.defaultMap
  y <- forAll EGen.rational
  annotateShow m
  annotateShow x1
  annotateShow x2
  annotateShow y
  let x1' :: Term s PInteger
      x1' = fromInteger x1
      x2' :: Term s PInteger
      x2' = fromInteger x2
      y' :: Term s PRational
      y' = fromRational y
      m1 :: Term s (Map PInteger PRational)
      m1 = marshal m
      m2 :: Term s (Map PInteger PRational)
      m2 = pinsert # x1' # y' # m1
      r1 :: Term s (PMaybe PRational)
      r1 = plookup # x2' # m1
      r2 :: Term s (PMaybe PRational)
      r2 = plookup # x2' # m2
  testPEq r1 r2

unionWithF :: (forall a. Num a => a -> a -> a) -> Property
unionWithF f =
  haskPlutEquiv
    viaBoth
    (M.unionWith @Integer @Rational f)
    (unionWith # plam f)
    (EGen.smallDomainMap, EGen.smallDomainMap)

unionWithAdd :: Property
unionWithAdd = unionWithF (+)

unionWithSub :: Property
unionWithSub = unionWithF (-)

filterWithKeyTest :: Property
filterWithKeyTest =
  haskPlutEquiv
    viaBoth
    (M.filterWithKey hf)
    (filterWithKey # pf)
    EGen.defaultMap
  where
    hf :: Integer -> Rational -> Bool
    hf x y = x < round y
    pf :: ClosedTerm (PInteger :--> PRational :--> PBool)
    pf = plam $ \x y -> x #< pround # y

findWithDefaultTest :: Property
findWithDefaultTest =
  haskPlutEquiv
    viaBoth
    (M.findWithDefault @Integer @Rational)
    findWithDefault
    (EGen.rational, EGen.integer, EGen.defaultMap)

adjustTest :: Property
adjustTest =
  haskPlutEquiv
    viaBoth
    (M.adjust @Integer @Rational (* 2))
    (adjust # plam (* 2))
    (EGen.integer, EGen.defaultMap)

mapWithKeyTest :: Property
mapWithKeyTest =
  haskPlutEquiv
    viaBoth
    (M.mapWithKey @Integer @Rational (\k v -> fromInteger k + v))
    (mapWithKey # plam (\k v -> pfromInteger # k + v))
    EGen.defaultMap

mallTest :: Property
mallTest =
  haskPlutEquiv
    viaData
    (all @(M.Map Integer) @Rational (> 0))
    (mall # plam (0 #<))
    EGen.defaultMap

msingletonTest :: Property
msingletonTest =
  haskPlutEquiv
    viaPEq
    (M.singleton @Integer @Rational)
    msingleton
    (EGen.integer, EGen.rational)

splitJoinTest :: Property
splitJoinTest =
  leftInverse
    viaPEq
    mapJoin
    mapSplit
    (EGen.mapOf (EGen.pairOf EGen.integer EGen.integer) EGen.rational)

mmapTest :: Property
mmapTest =
  haskPlutEquiv
    viaBoth
    (M.map (* 2) :: M.Map Integer Rational -> M.Map Integer Rational)
    (mmap # plam (* 2))
    EGen.defaultMap
