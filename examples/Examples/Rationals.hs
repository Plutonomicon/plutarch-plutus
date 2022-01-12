module Examples.Rationals (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch
import Plutarch.Bool
import Plutarch.Rational

--import Data.Ratio ((%))

tests :: HasTester => TestTree
tests = do
  testGroup
    "rational tests"
    [ testCase "1/2 + 1/2 = 1" $
        expect $ 1 / 2 + 1 / 2 #== (1 :: Term s PRational)
    , testCase "1/2 - 1/3 = 1/6" $
        expect $ 1 / 2 - 1 / 3 #== (1 / 6 :: Term s PRational)
    , testCase "2/9 < 3/10" $
        expect $ 2 / 9 #< (3 / 10 :: Term s PRational)
    , testCase "harmonic sum" $
        expect $ 1 / 2 + 1 / 3 + 1 / 4 + 1 / 5 #== (77 / 60 :: Term s PRational)
    , testCase "product" $
        expect $ 1 / 2 * 2 / 3 * 3 / 4 * 4 / 5 * 5 / 6 #== (1 / 6 :: Term s PRational)
    , testCase "0.5 literal" $
        printTerm (0.5 :: Term s PRational) @?= "(program 1.0.0 (\\i0 -> i1 1 2))"
        -- most print tests are impractical to read
        -- and varify by hand reduce is fairly
        -- complicated and used in even fairly short tests
    ]
