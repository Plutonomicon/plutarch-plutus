module Compiled (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pcompiled,
  plam',
 )
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Bool (PBool, pif)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenEval)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Compiled"
    [ plutarchGolden
        "\\cond ifT ifF -> (compiled (\\cond' ifT' ifF' -> if cond' ifT' ifF') cond ifT ifF"
        "Compiled Case 1"
        case1
    , plutarchGoldenEval
        "\\cond ifT ifF -> (compiled (\\cond' ifT' ifF' -> if cond' ifT' ifF') cond ifT ifF"
        "Compiled Case 1"
        case1
    ]

-- Cases

-- Case 1: \cond ifT ifF -> (compiled (\cond' ifT' ifF' -> pif cond' ifT' ifF') cond ifT ifF
case1 ::
  forall (s :: S).
  Term s (PBool :--> PInteger :--> PInteger :--> PInteger)
case1 = plam' $ \cond -> plam' $ \ifT -> plam' $ \ifF ->
  pcompiled go # cond # ifT # ifF
  where
    go ::
      forall (s' :: S).
      Term s' (PBool :--> PInteger :--> PInteger :--> PInteger)
    go = plam' $ \cond' -> plam' $ \ifT' -> plam' $ \ifF' -> pif cond' ifT' ifF'
