module Plutarch.BoolSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

-- TODO: Finish boolean tests
tests :: TestTree
tests =
  testGroup "bool" $
    [ testGroup "pnot" $
        mconcat
          [ goldens "pnot" pnot
          , goldens "pnot.app" (pnot #$ pcon PTrue)
          ,
            [ testCase "pnot.true" $ (pnot #$ pcon PTrue) #@?= pcon PFalse
            , testCase "pnot.false" $ (pnot #$ pcon PFalse) #@?= pcon PTrue
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
                [ testCase "tf" $ (pcon PTrue #&& pcon PFalse) #@?= pcon PFalse
                , testCase "ft" $ (pcon PFalse #&& pcon PTrue) #@?= pcon PFalse
                , testCase "tt" $ (pcon PTrue #&& pcon PTrue) #@?= pcon PTrue
                , testCase "ff" $ (pcon PFalse #&& pcon PFalse) #@?= pcon PFalse
                ]
              ]
        ]
    ]
