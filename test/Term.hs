module Term (goldens) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pdelay,
  pforce,
  plam',
  plet,
  punsafeConstant,
 )
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Bool (pfalse, pif, ptrue)
import Plutarch.Primitive.BuiltinFun (paddInteger)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenEval)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Term"
    [ plutarchGolden "\\x -> (\\y -> y) ((\\z -> z) x)" "Term Case 1" case1
    , plutarchGoldenEval "\\x -> (\\y -> y) ((\\z -> z) x)" "Term Case 1" case1
    , plutarchGolden "\\x -> force (delay x)" "Term Case 2" case2
    , plutarchGolden "(pif false 1 2) + (pif true 1 2)" "Term Case 3" case3
    , plutarchGolden "(pif false 1 2) + (pif true 1 2)" "Term Case 3 plet" case3Plet
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

-- Helpers

-- 'Integer constant'
ic :: forall (s :: S). Integer -> Term s PInteger
ic = punsafeConstant . PLC.someValue @Integer
