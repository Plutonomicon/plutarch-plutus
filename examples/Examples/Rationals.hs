module Examples.Rationals (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Pair
import Plutarch.Rational

--import Data.Ratio ((%))

tests :: HasTester => TestTree
tests = do
  testGroup
    "rational tests"
    [ testCase "1/2 + 1/2 = 1" $
        expect $ 1 / 2 + 1 / 2 #== (1 :: Term s PRational)
    , testCase "(1 - 3/2) * (2 - 5/2) == 1/4" $
        expect $ (1 - 3/2) * (2 - 5/2) #== (1/4 :: Term s PRational)
    , testCase "1/2 - 1/3 = 1/6" $
        expect $ 1 / 2 - 1 / 3 #== (1 / 6 :: Term s PRational)
    , testCase "2/9 < 3/10" $
        expect $ 2 / 9 #< (3 / 10 :: Term s PRational)
    , testCase "harmonic sum" $
        expect $ 1 / 2 + 1 / 3 + 1 / 4 + 1 / 5 #== (77 / 60 :: Term s PRational)
    , testCase "product" $
        expect $ 1 / 2 * 2 / 3 * 3 / 4 * 4 / 5 * 5 / 6 #== (1 / 6 :: Term s PRational)
    , testCase "round 5/3" $
        expect $ pround # (5 / 3) #== 2
    , testCase "round 4/3" $
        expect $ pround # (4 / 3) #== 1
    , testCase "round 5/2" $
        expect $ pround # (5 / 2) #== 2
    , testCase "round 7/2" $
        expect $ pround # (7 / 2) #== 4
    , testCase "round 9/2" $
        expect $ pround # (9 / 2) #== 4
    , testCase "round 11/2" $
        expect $ pround # (11 / 2) #== 6
    , testCase "round 9/4" $
        expect $ pround # (9 / 4) #== 2
    , testCase "round 11/4" $
        expect $ pround # (11 / 4) #== 3
    , testCase "round -1/3" $
        expect $ pround # (-1 / 3) #== 0
    , testCase "round -1/2" $
        expect $ pround # (-1 / 2) #== 0
    , testCase "round -2/3" $
        expect $ pround # (-2 / 3) #== -1
    , testCase "round -3/2" $
        expect $ pround # (-3 / 2) #== -2
    , testCase "round -5/2" $
        expect $ pround # (-5 / 2) #== -2
    , testCase "truncate 5/4" $
        expect $ ptruncate # (5 / 4) #== 1
    , testCase "truncate 3/2" $
        expect $ ptruncate # (3 / 2) #== 1
    , testCase "truncate 7/4" $
        expect $ ptruncate # (7 / 4) #== 1
    , testCase "truncate 1/4" $
        expect $ ptruncate # (1 / 4) #== 0
    , testCase "truncate -1/4" $
        expect $ ptruncate # (-1 / 4) #== 0
    , testCase "truncate -7/4" $
        expect $ ptruncate # (-7 / 4) #== -1
    , testCase "properFraction 11/7" $
        expect $
          pmatch (pproperFraction # (11 / 7)) $ \(PPair x y) ->
            x #== 1 #&& y #== (4 / 7)
    , testCase "properFraction 13/7" $
        expect $
          pmatch (pproperFraction # (13 / 7)) $ \(PPair x y) ->
            x #== 1 #&& y #== (6 / 7)
    , testCase "properFraction -1/2" $
        expect $
          pmatch (pproperFraction # (-1 / 2)) $ \(PPair x y) ->
            x #== 0 #&& y #== (-1 / 2)
    , testCase "properFraction -3/2" $
        expect $
          pmatch (pproperFraction # (-3 / 2)) $ \(PPair x y) ->
            x #== -1 #&& y #== (-1 / 2)
    , testCase "properFraction -4/3" $
        expect $
          pmatch (pproperFraction # (-4 / 3)) $ \(PPair x y) ->
            x #== -1 #&& y #== (-1 / 3)
    , testCase "0.5 literal" $
        printTerm (0.5 :: Term s PRational) @?= "(program 1.0.0 (\\i0 -> i1 1 2))"
    , -- most print tests are impractical to read
      -- and varify by hand because reduce is fairly
      -- complicated and used in even fairly short tests
      testCase "pfromData . pdata = id" $ do
        expect $ (0.5 :: Term s PRational) #== pfromData (pdata 0.5)
        expect $ (2 :: Term s PRational) #== pfromData (pdata 2)
        expect $ (3 :: Term s PRational) #== pfromData (pdata 3)
        expect $ ((1 / 3) :: Term s PRational) #== pfromData (pdata (1 / 3))
        expect $ ((11 / 7) :: Term s PRational) #== pfromData (pdata (11 / 7))
    ]
