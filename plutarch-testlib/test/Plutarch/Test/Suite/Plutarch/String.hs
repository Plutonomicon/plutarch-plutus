module Plutarch.Test.Suite.Plutarch.String (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

{-# HLINT ignore tests "Monoid law, left identity" #-}
tests :: TestTree
tests =
  testGroup
    "String"
    [ plutarchGolden
        "Goldens"
        "str"
        [ goldenAssertEqual "eq" ("foo" #== pconstant @PString "foo") (pcon PTrue)
        , goldenGroup
            "semigroup"
            [ goldenGroup
                "laws"
                [ goldenAssertEqual "id.1" ((mempty <> pconstant @PString "foo") #== pconstant @PString "foo") (pcon PTrue)
                , goldenAssertEqual "id.2" (pconstant @PString "foo" #== (mempty <> pconstant @PString "foo")) (pcon PTrue)
                ]
            , goldenAssertEqual "concat" ((pconstant @PString "foo" <> pconstant @PString "bar") #== pconstant @PString "foobar") (pcon PTrue)
            , goldenAssertEqual "mempty" (mempty #== pconstant @PString "") (pcon PTrue)
            ]
        ]
    ]
