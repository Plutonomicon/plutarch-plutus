module Bool (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
 )
import Plutarch.Primitive.Bool (PBool, pnot, por)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Test.Golden (plutarchGolden)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Bool"
    [ plutarchGolden "\\x y -> or (not x) y" "Bool Case 1" case1
    ]

-- Cases

-- Case 1: \x y -> por (pnot x) y
case1 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
case1 = plam' $ \x -> plam' $ \y -> por (pnot x) y
