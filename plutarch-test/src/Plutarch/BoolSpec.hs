module Plutarch.BoolSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

tests :: TestTree
tests =
  testGroup "bool" $
    [ testGroup "pnot" $
        mconcat
          [ goldens "pnot" pnot
          , goldens "pnot.app" (pnot #$ pcon PTrue)
          ,
            [ testCase "pnot.true" $ (pnot #$ pcon PTrue) `equal` pcon PFalse
            , testCase "pnot.false" $ (pnot #$ pcon PFalse) `equal` pcon PTrue
            ]
          ]
    , testGroup
        "lazy"
        [ testGroup "pand" $
            mconcat
              [ goldens "lazy.pand.tf" (pcon PTrue #&& pcon PFalse)
              , goldens "lazy.pand.ft" (pcon PFalse #&& pcon PTrue)
              , goldens "lazy.pand.tt" (pcon PTrue #&& pcon PTrue)
              ,
                [ testCase "tf" $ (pcon PTrue #&& pcon PFalse) `equal` pcon PFalse
                , testCase "ft" $ (pcon PFalse #&& pcon PTrue) `equal` pcon PFalse
                , testCase "tt" $ (pcon PTrue #&& pcon PTrue) `equal` pcon PTrue
                , testCase "ff" $ (pcon PFalse #&& pcon PFalse) `equal` pcon PFalse
                ]
              ]
        ]
    ]
