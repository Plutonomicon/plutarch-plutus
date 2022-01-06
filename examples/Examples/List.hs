module Examples.List (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch
import Plutarch.Bool (pnot, (#==), (#<))
import Plutarch.Integer
import Plutarch.List
import Plutarch.Builtin (PBuiltinList(..))

--------------------------------------------------------------------------------

integerList :: [Integer] -> Term s (PScottList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

tests :: TestTree
tests = do
  testGroup "List tests" $
    [ testCase "pconcat identities" $ do
        let xs :: Term s (PScottList PInteger)
            xs = psingleton # (fromInteger @(Term _ PInteger) 0)
        expect $ (pconcat # xs # pnilList) #== xs
    , testCase "pmap" $ do
        let xs :: Term _ (PScottList PInteger)
            xs = integerList [1 .. 10]
        expect $
          pmap # (plam $ \x -> x + x) # xs
            #== (integerList $ fmap (* 2) [1 .. 10])
        expect $ pmap @PScottList # (plam $ \(x :: Term _ PInteger) -> x) # pnilList #== pnilList
    , testCase "pfilter" $ do
        let xs :: Term _ (PScottList PInteger)
            xs = integerList [1 .. 10]
        expect $
          (pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs)
            #== (integerList [2, 4, 6, 8, 10])
        expect $
          (pfilter # (plam $ \x -> 5 #< x) # xs)
            #== (integerList [6 .. 10])
    , testCase "punsafeHead" $
        expect $ (punsafeHead # integerList [1 .. 10]) #== 1
    , testCase "punsafeTail" $
        expect $ (punsafeTail # integerList [1 .. 10]) #== integerList [2 .. 10]
    , testCase "pnull" $ do
        expect $ pnot #$ pnull # integerList [1 .. 10]
        expect $ pnull # integerList []
    , testCase "pzipWith" $ do
        expect $ (pzipWith' (+) # integerList [1..10] # integerList [1..10])
             #== integerList (fmap (*2) [1..10])
    ]
