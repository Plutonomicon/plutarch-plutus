module Term (goldens) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pdelay,
  pfix,
  pforce,
  plam',
  plet,
  punsafeConstant,
 )
import Plutarch.Numeric.Additive ((#+))
import Plutarch.Numeric.Multiplicative ((#*))
import Plutarch.Primitive.Apply ((#), (#$))
import Plutarch.Primitive.Bool (PBool, pfalse, pif, ptrue)
import Plutarch.Primitive.BuiltinFun (paddInteger, pequalsInteger)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenAll)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Term"
    [ plutarchGoldenAll "\\x -> (\\y -> y) ((\\z -> z) x)" "Term Case 1" case1
    , plutarchGolden "\\x -> force (delay x)" "Term Case 2" case2
    , plutarchGolden "(pif false 1 2) + (pif true 1 2)" "Term Case 3" case3
    , plutarchGolden "(pif false 1 2) + (pif true 1 2)" "Term Case 3 plet" case3Plet
    , plutarchGoldenAll "pif a then (b c) else (d e)" "Term Case 4" case4
    , plutarchGoldenAll "sum 0 .. 9" "Term Case 5" case5
    ]

-- Cases

-- Case 1: \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 = plam' $ \x -> plam' id # (plam' id # x)

-- Case 2: \x -> force (delay x)
case2 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case2 = plam' $ \x -> pforce (pdelay x)

case3 :: forall (s :: S). Term s PInteger
case3 =
  let f = plam' $ \x -> pif x (ic 1) (ic 2)
   in paddInteger # (f # ptrue) # (f # pfalse)

case3Plet :: forall (s :: S). Term s PInteger
case3Plet = plet (plam' $ \x -> pif x (ic 1) (ic 2)) $ \f ->
  paddInteger # (f # ptrue) # (f # pfalse)

case4 :: forall (s :: S). Term s (PBool :--> PInteger)
case4 = plam' $ \b -> pif b (squares # ic 10 # ic 20) (sums # ic 10 # ic 20)
  where
    squares :: Term s (PInteger :--> PInteger :--> PInteger)
    squares = plam' $ \i -> plam' $ \j -> (square # i) #+ (square # j)
    sums :: Term s (PInteger :--> PInteger :--> PInteger)
    sums = plam' $ \i -> plam' $ \j -> (double # i) #+ (double # j)
    square :: Term s (PInteger :--> PInteger)
    square = plam' $ \x -> x #* x
    double :: Term s (PInteger :--> PInteger)
    double = plam' $ \x -> x #+ x

case5 :: forall (s :: S). Term s PInteger
case5 = go # ic 0
  where
    go :: Term s (PInteger :--> PInteger)
    go = pfix $ \self -> plam' $ \i ->
      pif
        (pequalsInteger # i # ic 10)
        (ic 0)
        (paddInteger # i #$ self #$ paddInteger # i # ic 1)

-- Helpers

-- 'Integer constant'
ic :: forall (s :: S). Integer -> Term s PInteger
ic = punsafeConstant . PLC.someValue @Integer
