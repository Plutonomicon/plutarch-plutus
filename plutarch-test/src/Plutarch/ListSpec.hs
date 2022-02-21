module Plutarch.ListSpec (spec) where

import Test.Syd

import Plutarch.List (pconvertLists, pfoldl')
import Plutarch.Prelude
import Plutarch.Test

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

spec :: Spec
spec = do
  describe "list" $ do
    let xs10 :: Term _ (PList PInteger)
        xs10 = integerList [1 .. 10]
    describe "type" . pgoldenSpec $ do
      "phead" @| phead # xs10 @== pconstant @PInteger 1
      "ptail" @| ptail # xs10 @== integerList [2 .. 10]
      let matchP = pmatch (integerList [1, 3, 1]) $ \case
            PSNil -> perror
            PSCons x _ -> x
      "pmatch" @| matchP @== pconstant @PInteger 1
    describe "fun" . pgoldenSpec $ do
      "pnull" @\ do
        "empty" @| pnull # (integerList []) @-> passert
        "nonempty" @| pnot # (pnull # xs10) @-> passert
      "pconcat" @\ do
        let xs :: Term s (PList PInteger)
            xs = psingleton # (fromInteger @(Term _ PInteger) 0)
        "identity" @| pconcat # xs # pnil @== xs
      "pmap" @\ do
        "eg" @| pmap # (plam $ \x -> x + x) # xs10
          @== (integerList $ fmap (* 2) [1 .. 10])
        "identity" @| pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil
          @== pnil @PList
      "pfilter" @\ do
        "1" @| pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs10
          @== integerList [2, 4, 6, 8, 10]
        "2" @| pfilter # (plam $ \x -> 5 #< x) # xs10
          @== integerList [6 .. 10]
      "pzipWith" @| pzipWith' (+) # xs10 # xs10
        @== (integerList (fmap (* 2) [1 .. 10]))
      "pfoldl" @\ do
        "primed" @\ do
          "nonempty" @| pfoldl' (-) # 0 # xs10
            @== pconstant @PInteger (foldl (-) 0 [1 .. 10])
          "empty" @| pfoldl' (-) # 0 # integerList []
            @== pconstant @PInteger 0
        "primed" @\ do
          "nonempty" @| pfoldl # plam (-) # 0 # xs10
            @== pconstant @PInteger (foldl (-) 0 [1 .. 10])
          "empty" @| pfoldl # plam (-) # 0 # integerList []
            @== pconstant @PInteger 0
