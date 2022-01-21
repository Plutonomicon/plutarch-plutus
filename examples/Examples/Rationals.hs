module Examples.Rationals (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Lift (pconstant)
import Plutarch.Numeric (
  PAdditiveGroup ((#-)),
  PAdditiveSemigroup ((#+)),
  PMultiplicativeMonoid (pone),
  PMultiplicativeSemigroup ((#*)),
 )
import Plutarch.Pair
import Plutarch.Rational

-- import Data.Ratio ((%))

tests :: HasTester => TestTree
tests = do
  testGroup
    "rational tests"
    [ testCase "1/2 + 1/2 = 1" $
        expect $
          pconstant 1 #% pconstant 2
            #+ pconstant 1 #% pconstant 2
            #== pconstant 1 #% pconstant 1
    , testCase "(1 - 3/2) * (2 - 5/2) == 1/4" $
        expect $
          (pone #- pconstant 3 #% pconstant 2)
            #* (pconstant 2 #% pconstant 1 #- pconstant 5 #% pconstant 2)
            #== pconstant 1 #% pconstant 4
    , testCase "1/2 - 1/3 = 1/6" $
        expect $
          pconstant 1 #% pconstant 2 #- pconstant 1 #% pconstant 3 #== (pconstant 1 #% pconstant 6 :: Term s PRational)
    , testCase "2/9 < 3/10" $
        expect $ pconstant 2 #% pconstant 9 #< (pconstant 3 #% pconstant 10 :: Term s PRational)
    , testCase "harmonic sum" $
        expect $ pconstant 1 #% pconstant 2 #+ pconstant 1 #% pconstant 3 #+ pconstant 1 #% pconstant 4 #+ pconstant 1 #% pconstant 5 #== (pconstant 77 #% pconstant 60 :: Term s PRational)
    , testCase "product" $
        expect $
          pconstant 1 #% pconstant 2
            #* pconstant 2 #% pconstant 3
            #* pconstant 3 #% pconstant 4
            #* pconstant 4 #% pconstant 5
            #* pconstant 5 #% pconstant 6
            #== (pconstant 1 #% pconstant 6)
    , testCase "round 5/3" $
        expect $ pround # (pconstant 5 #% pconstant 3) #== pconstant 2
    , testCase "round 4/3" $
        expect $ pround # (pconstant 4 #% pconstant 3) #== pconstant 1
    , testCase "round 5/2" $
        expect $ pround # (pconstant 5 #% pconstant 2) #== pconstant 2
    , testCase "round 7/2" $
        expect $ pround # (pconstant 7 #% pconstant 2) #== pconstant 4
    , testCase "round 9/2" $
        expect $ pround # (pconstant 9 #% pconstant 2) #== pconstant 4
    , testCase "round 11/2" $
        expect $ pround # (pconstant 11 #% pconstant 2) #== pconstant 6
    , testCase "round 9/4" $
        expect $ pround # (pconstant 9 #% pconstant 4) #== pconstant 2
    , testCase "round 11/4" $
        expect $ pround # (pconstant 11 #% pconstant 4) #== pconstant 3
    , testCase "round -1/3" $
        expect $ pround # (pconstant (-1) #% pconstant 3) #== pconstant 0
    , testCase "round -1/2" $
        expect $ pround # (pconstant (-1) #% pconstant 2) #== pconstant 0
    , testCase "round -2/3" $
        expect $ pround # (pconstant (-2) #% pconstant 3) #== pconstant (-1)
    , testCase "round -3/2" $
        expect $ pround # (pconstant (-3) #% pconstant 2) #== pconstant (-2)
    , testCase "round -5/2" $
        expect $ pround # (pconstant (-5) #% pconstant 2) #== pconstant (-2)
    , testCase "truncate 5/4" $
        expect $ ptruncate # (pconstant 5 #% pconstant 4) #== pconstant 1
    , testCase "truncate 3/2" $
        expect $ ptruncate # (pconstant 3 #% pconstant 2) #== pconstant 1
    , testCase "truncate 7/4" $
        expect $ ptruncate # (pconstant 7 #% pconstant 4) #== pconstant 1
    , testCase "truncate 1/4" $
        expect $ ptruncate # (pconstant 1 #% pconstant 4) #== pconstant 0
    , testCase "truncate -1/4" $
        expect $ ptruncate # (pconstant (-1) #% pconstant 4) #== pconstant 0
    , testCase "truncate -7/4" $
        expect $ ptruncate # (pconstant (-7) #% pconstant 4) #== pconstant (-1)
    , testCase "properFraction 11/7" $
        expect $
          pmatch (pproperFraction # (pconstant 11 #% pconstant 7)) $ \(PPair x y) ->
            x #== pconstant 1 #&& y #== (pconstant 4 #% pconstant 7)
    , testCase "properFraction 13/7" $
        expect $
          pmatch (pproperFraction # (pconstant 13 #% pconstant 7)) $ \(PPair x y) ->
            x #== pconstant 1 #&& y #== (pconstant 6 #% pconstant 7)
    , testCase "properFraction -1/2" $
        expect $
          pmatch (pproperFraction # (pconstant (-1) #% pconstant 2)) $ \(PPair x y) ->
            x #== pconstant 0 #&& y #== (pconstant (-1) #% pconstant 2)
    , testCase "properFraction -3/2" $
        expect $
          pmatch (pproperFraction # (pconstant (-3) #% pconstant 2)) $ \(PPair x y) ->
            x #== pconstant (-1) #&& y #== (pconstant (-1) #% pconstant 2)
    , testCase "properFraction -4/3" $
        expect $
          pmatch (pproperFraction # (pconstant (-4) #% pconstant 3)) $ \(PPair x y) ->
            x #== pconstant (-1) #&& y #== (pconstant (-1) #% pconstant 3)
    , -- most print tests are impractical to read
      -- and varify by hand because reduce is fairly
      -- complicated and used in even fairly short tests
      testCase "pfromData . pdata = id" $ do
        expect $
          (pconstant 1 #% pconstant 2)
            #== (pfromData . pdata $ pconstant 1 #% pconstant 2)
        expect $
          (pconstant 2 #% pconstant 1)
            #== (pfromData . pdata $ pconstant 2 #% pconstant 1)
        expect $
          (pconstant 3 #% pconstant 1)
            #== (pfromData . pdata $ pconstant 3 #% pconstant 1)
        expect $
          (pconstant 1 #% pconstant 3)
            #== (pfromData . pdata $ pconstant 1 #% pconstant 3)
        expect $
          (pconstant 11 #% pconstant 7)
            #== (pfromData . pdata $ pconstant 11 #% pconstant 7)
    ]
