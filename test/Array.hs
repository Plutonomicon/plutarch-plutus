module Array (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeConstant,
 )
import Plutarch.Numeric.Additive (padd, (#+))
import Plutarch.Numeric.Multiplicative (pone)
import Plutarch.Prelude.PullArray (
  PPullArray,
  pimapArray,
  piota,
  pmapArray,
 )
import Plutarch.Primitive.Numeric (PNatural)
import Plutarch.Test.Golden (plutarchGolden)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Array"
    [ plutarchGolden "map (+ 1) . map (+ 1) . piota $ 10" "Array Case 1" case1
    , plutarchGolden "imap (+) . imap (+) . piota $ 10" "Array Case 2" case2
    , plutarchGolden "piota 10" "Array Case 3" case3
    ]

-- Cases

case1 :: forall (s :: S). Term s (PPullArray PNatural)
case1 =
  pmapArray (plam' (#+ pone))
    . pmapArray (plam' (#+ pone))
    . piota
    $ ten

case2 :: forall (s :: S). Term s (PPullArray PNatural)
case2 =
  pimapArray padd . pimapArray padd . piota $ ten

case3 :: forall (s :: S). Term s (PPullArray PNatural)
case3 = piota ten

-- Helpers

ten :: forall (s :: S). Term s PNatural
ten = punsafeConstant $ PLC.someValue @Integer 10
