module Plutarch.RecursionSpec (spec) where 

import Prelude hiding (succ)
  
import Plutarch
import Plutarch.Bool (pif, (#==))
import Plutarch.Integer (PInteger)

import Test.Syd (Spec, describe, it)

import Plutarch.Test

spec :: Spec
spec = do  
  describe "recursion" $ do 
    describe "example" $ do 
      -- compilation
      describe "iterateN" $ 
        golden All iterateN
      -- tests
      describe "iterateN (10) (+1) 0 == 10" $ do
        let p :: Term s PInteger
            p = 10
        it "works" $ (iterateN # 10 # succ # 0) #@?= p
      describe "iterateN 10 (*2) 1 == 1024" $ do 
        let p :: Term s PInteger
            p = 1024
        it "works" $ (iterateN # 10 # double # 1) #@?= p

succ :: Term s (PInteger :--> PInteger) 
succ = plam $ \x -> x + 1

double :: Term s (PInteger :--> PInteger) 
double = plam $ \x -> x * 2

{- |
  Example of a simple recursive function with pfix
-}
iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
iterateN = pfix # plam iterateN'
  where
    iterateN' ::
      Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
      Term s PInteger ->
      Term s (a :--> a) ->
      Term s a ->
      Term s a

    iterateN' self n f x =
      pif
        (n #== 0)
        x
        (self # (n - 1) # f #$ f # x)
