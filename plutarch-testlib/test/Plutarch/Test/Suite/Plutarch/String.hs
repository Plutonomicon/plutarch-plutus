module Plutarch.Test.Suite.Plutarch.String (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEvalEqual, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

{-# HLINT ignore tests "Monoid law, left identity" #-}
tests :: TestTree
tests =
  testGroup
    "String"
    [ plutarchGolden
        "Goldens"
        "str"
        [ goldenEvalEqual "eq" ("foo" #== pconstant @PString "foo") (pcon PTrue)
        , goldenGroup
            "semigroup"
            [ goldenGroup
                "laws"
                [ goldenEvalEqual "id.1" ((mempty <> pconstant @PString "foo") #== pconstant @PString "foo") (pcon PTrue)
                , goldenEvalEqual "id.2" (pconstant @PString "foo" #== (mempty <> pconstant @PString "foo")) (pcon PTrue)
                ]
            , goldenEvalEqual "concat" ((pconstant @PString "foo" <> pconstant @PString "bar") #== pconstant @PString "foobar") (pcon PTrue)
            , goldenEvalEqual "mempty" (mempty #== pconstant @PString "") (pcon PTrue)
            ]
        ]
    ]
