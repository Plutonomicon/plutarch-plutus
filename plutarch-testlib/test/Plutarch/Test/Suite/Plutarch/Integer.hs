module Plutarch.Test.Suite.Plutarch.Integer (tests) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Integer"
    [ plutarchGolden
        "Goldens"
        "int.examples"
        [ goldenEval "add1" add1
        , goldenEval "add1Hoisted" add1Hoisted
        , goldenEval "example1" example1
        , goldenEval "example2" example2
        , goldenGroup
            "fib"
            [ goldenEval "lam" fib
            , goldenGroup
                "app"
                [ goldenAssertEqual "9" (fib # 9) (pconstant 34)
                ]
            ]
        , goldenEval "uglyDouble" uglyDouble
        ]
    ]

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = plam $ \x y -> x + y + 1

add1Hoisted :: Term s (PInteger :--> PInteger :--> PInteger)
add1Hoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

example1 :: Term s PInteger
example1 = add1Hoisted # 12 # 32 + add1Hoisted # 5 # 4

example2 :: Term s (PEither PInteger PInteger :--> PInteger)
example2 = plam $ \x -> pmatch x $ \case
  PLeft n -> n + 1
  PRight n -> n - 1

fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
      $ self # (n - 1) + self # (n - 2)

uglyDouble :: Term s (PInteger :--> PInteger)
uglyDouble = plam $ \n -> plet n $ \n1 -> plet n1 $ \n2 -> n2 + n2
