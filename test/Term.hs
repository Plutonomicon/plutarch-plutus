module Term (goldens) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pdelay,
  pforce,
  plam',
 )
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenAll)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Term"
    [ plutarchGoldenAll "\\x -> (\\y -> y) ((\\z -> z) x)" "Term Case 1" case1
    , plutarchGolden "\\x -> force (delay x)" "Term Case 2" case2
    ]

-- Cases

-- Case 1: \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 = plam' $ \x -> plam' id # (plam' id # x)

-- Case 2: \x -> force (delay x)
case2 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case2 = plam' $ \x -> pforce (pdelay x)
