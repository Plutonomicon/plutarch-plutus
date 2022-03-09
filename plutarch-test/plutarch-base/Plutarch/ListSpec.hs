module Plutarch.ListSpec (spec, integerList) where

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
      "plength"
        @| plength # xs10 @== pconstant @PInteger 10
      "pelem"
        @| pelem # 5 # xs10 @-> passert
      "pall"
        @| pall # plam (const $ pconstant @PBool False) # xs10 @-> passertNot
      "plistEquals" @\ do
        "true" @| plistEquals # xs10 # integerList [1 .. 10] @-> passert
        "false" @| plistEquals # xs10 # integerList [1 .. 3] @-> passertNot
        "empty" @| plistEquals # xs10 # integerList [] @-> passertNot
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
    -- Benchmarking different ways of using lists
    describe "bench" . pgoldenSpec $ do
      let numList = pconstant @(PBuiltinList PInteger) [1 .. 5]
      -- Two ways of matching on a list
      "x1+x2" @\ do
        -- Via HeadList and TailList only.
        "builtin" @| (phead #$ ptail # numList) + (phead # numList)
        -- Via ChooseList (twice invoked)
        "pmatch"
          @| pmatch numList
          $ \case
            PNil -> perror
            PCons x xs ->
              pmatch xs $ \case
                PNil -> perror
                PCons y _ ->
                  x + y
      -- Various ways of uncons'ing a list
      "uncons" @\ do
        -- ChooseList builtin, like uncons but fails on null lists
        "ChooseList"
          @| pmatch numList
          $ \case
            PNil -> perror
            PCons _x xs ->
              xs
        -- Retrieving head and tail of a list
        "head-and-tail"
          @| plet (phead # numList)
          $ \_x ->
            ptail # numList
        -- Retrieve head and tail using builtins, but fail on null lists.
        "head-and-tail-and-null"
          @| plet (pnull # numList)
          $ \isEmpty ->
            pmatch isEmpty $ \case
              PTrue -> perror
              PFalse -> plet (phead # numList) $ \_x ->
                ptail # numList
