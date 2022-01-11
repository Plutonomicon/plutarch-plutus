module Examples.List (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch
import Plutarch.Bool (pnot, (#<), (#==))
import Plutarch.Builtin (PBuiltinList (..))
import Plutarch.Integer
import Plutarch.List

--------------------------------------------------------------------------------

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

tests :: HasTester => TestTree
tests = do
  testGroup "List tests" $
    [ testCase "pconcat identities" $ do
        let xs :: Term s (PList PInteger)
            xs = psingleton # (fromInteger @(Term _ PInteger) 0)
        expect $ (pconcat # xs # pnil) #== xs
    , testCase "pmap" $ do
        let xs :: Term _ (PList PInteger)
            xs = integerList [1 .. 10]
        expect $
          pmap # (plam $ \x -> x + x) # xs
            #== (integerList $ fmap (* 2) [1 .. 10])
        expect $ pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil #== pnil
    , testCase "pfilter" $ do
        let xs :: Term _ (PList PInteger)
            xs = integerList [1 .. 10]
        expect $
          (pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs)
            #== (integerList [2, 4, 6, 8, 10])
        expect $
          (pfilter # (plam $ \x -> 5 #< x) # xs)
            #== (integerList [6 .. 10])
    , testCase "phead" $
        expect $ (phead # integerList [1 .. 10]) #== 1
    , testCase "ptail" $
        expect $ (ptail # integerList [1 .. 10]) #== integerList [2 .. 10]
    , testCase "pnull" $ do
        expect $ pnot #$ pnull # integerList [1 .. 10]
        expect $ pnull # integerList []
    , testCase "pzipWith" $ do
        expect $
          (pzipWith' (+) # integerList [1 .. 10] # integerList [1 .. 10])
            #== integerList (fmap (* 2) [1 .. 10])
    , testCase "pfoldl" $ do
        expect $
          (pfoldl # plam (-) # 0 # integerList [1 .. 10])
            #== pconstant (foldl (-) 0 [1 .. 10])
        expect $
          (pfoldl' (-) # 0 # integerList [1 .. 10])
            #== pconstant (foldl (-) 0 [1 .. 10])
        expect $
          (pfoldl # plam (-) # 0 # integerList [])
            #== pconstant 0
        expect $
          (pfoldl' (-) # 0 # integerList [])
            #== pconstant 0
    ]
