module Plutarch.ListSpec (spec) where

import Test.Syd

import Plutarch.List
import Plutarch.Prelude
import Plutarch.Test

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

spec :: Spec
spec = do
  describe "list" $ do
    let xs10 :: Term _ (PList PInteger)
        xs10 = integerList [1 .. 10]
    describe "pmatch" $ do
      let p = pmatch (integerList [1, 3, 1]) $ \_ -> perror
      golden PrintTermNoEval p
    describe "phead" $ do
      let p = phead # xs10
      golden All p
      it "works" $ passert $ p #== 1
    describe "ptail" $ do
      let p = ptail # xs10
      golden All p
      it "works" $ passert $ p #== integerList [2 .. 10]
    describe "pnull" $ do
      let p0 = pnull # integerList []
          p1 = pnull # xs10
      goldens All [("p0", p0), ("p1", p1)]
      it "empty" $ passert p0
      it "nonempty" $ passert $ pnot # p1
    describe "pconcat" $ do
      describe "identity" $ do
        let xs :: Term s (PList PInteger)
            xs = psingleton # (fromInteger @(Term _ PInteger) 0)
            p = pconcat # xs # pnil
        golden All p
        it "works" $ passert $ p #== xs
    describe "pmap" $ do
      let p = pmap # (plam $ \x -> x + x) # xs10
      golden All p
      it "eg" $ passert $ p #== (integerList $ fmap (* 2) [1 .. 10])
      it "identity" $ passert $ pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil #== pnil
    describe "pfilter" $ do
      let p1 = pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs10
          p2 = pfilter # (plam $ \x -> 5 #< x) # xs10
      goldens All [("p1", p1), ("p2", p2)]
      it "p1" $ passert $ p1 #== integerList [2, 4, 6, 8, 10]
      it "p2" $ passert $ p2 #== integerList [6 .. 10]
    describe "pzipWith" $ do
      let p = pzipWith' (+) # xs10 # xs10
      golden All p
      it "works" $ passert $ p #== integerList (fmap (* 2) [1 .. 10])
    describe "pfoldl" $ do
      let p1 = pfoldl # plam (-) # 0 # xs10
          p1' = pfoldl' (-) # 0 # xs10
          p2 = pfoldl # plam (-) # 0 # integerList []
          p2' = pfoldl' (-) # 0 # integerList []
      goldens All [("p1", p1), ("p1'", p1'), ("p2", p2), ("p2'", p2)]
      it "nonempty" $ passert $ p1 #== pconstant (foldl (-) 0 [1 .. 10])
      it "nonempty'" $ passert $ p1' #== pconstant (foldl (-) 0 [1 .. 10])
      it "empty" $ passert $ p2 #== pconstant 0
      it "empty'" $ passert $ p2' #== pconstant 0
