module Numeric (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
 )
import Plutarch.Numeric.Euclidean (pgcd)
import Plutarch.Numeric.Multiplicative (ppowNatural)
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.BuiltinFun (paddInteger, pmultiplyInteger)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGoldenAll)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Numeric"
    [ plutarchGoldenAll "\\x y -> addInteger x y" "Numeric Case 1" case1
    , plutarchGoldenAll
        "\\x y -> addInteger (multiplyInteger x x) (multiplyInteger y y)"
        "Numeric Case 2"
        case2
    , plutarchGoldenAll "ppowNatural @PInteger" "Numeric Case 3" (ppowNatural @PInteger)
    , plutarchGoldenAll "pgcd @PInteger" "Numeric Case 4" (pgcd @PInteger)
    ]

-- Cases

-- Case 1: \x y -> addInteger x y
case1 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
case1 = plam' $ \x -> plam' $ \y -> paddInteger # x # y

-- Case 2: \x y -> addInteger (multiplyInteger x x) (multiplyInteger y y)
case2 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
case2 = plam' $ \x -> plam' $ \y ->
  paddInteger # (pmultiplyInteger # x # x) # (pmultiplyInteger # y # y)
