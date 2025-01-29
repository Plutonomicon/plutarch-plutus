module Plutarch.Test.Suite.Plutarch.String (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

{-# HLINT ignore tests "Monoid law, left identity" #-}
tests :: TestTree
tests =
  testGroup
    "String"
    [ plutarchGolden
        "Goldens"
        "str"
        [ goldenEval "eq" ("foo" #== pconstant @PString "foo")
        , goldenGroup
            "semigroup"
            [ goldenGroup
                "laws"
                [ goldenEval "id.1" ((mempty <> pconstant @PString "foo") #== pconstant @PString "foo")
                , goldenEval "id.2" (pconstant @PString "foo" #== (mempty <> pconstant @PString "foo"))
                ]
            , goldenEval "concat" ((pconstant @PString "foo" <> pconstant @PString "bar") #== pconstant @PString "foobar")
            , goldenEval "mempty" (mempty #== pconstant @PString "")
            ]
        ]
    ]
