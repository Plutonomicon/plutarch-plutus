module Main (main) where

import Test.Tasty

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

import qualified Plutarch.BoolSpec as BoolSpec

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "main"
    [ testGroup "add1" $
        mconcat
          [ goldens "add1" add1
          , goldens "add1.app" $ add1 # 1 # 2
          ]
    , BoolSpec.tests
    ]

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = plam $ \x y -> x + y + 1
