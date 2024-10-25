module Plutarch.Test.Suite.Plutarch.List (tests, integerList) where

import Data.List (find)
import Plutarch.LedgerApi.Utils (pmaybeToMaybeData)
import Plutarch.List (pconvertLists, pfoldl')
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenAssertFail, goldenEval, goldenGroup, plutarchGolden)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  forAllShrinkShow,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

tests :: TestTree
tests =
  testGroup
    "List"
    [ plutarchGolden
        "Goldens"
        "list"
        [ goldenAssertFail "pmatch" (pmatch (integerList [1, 3, 1]) (const perror))
        , goldenAssertEqual "phead" (1 #== (phead # xs10)) (pcon PTrue)
        , goldenAssertEqual "ptail" (integerList [2 .. 10] #== ptail # xs10) (pcon PTrue)
        , goldenGroup
            "pnull"
            [ goldenAssertEqual "empty" (pnull # integerList []) (pcon PTrue)
            , goldenAssertEqual "nonempty" (pnot #$ pnull # xs10) (pcon PTrue)
            ]
        , goldenGroup
            "pconcat"
            [ goldenAssertEqual "identity" ((pconcat # xs10 # pnil #== pconcat # pnil # xs10) #&& (pconcat # pnil # xs10 #== xs10)) (pcon PTrue)
            ]
        , goldenGroup
            "pmap"
            [ goldenAssertEqual "eg" ((pmap # plam (\x -> x + x) # xs10) #== integerList (fmap (* 2) [1 .. 10])) (pcon PTrue)
            , goldenAssertEqual "identity" ((pmap @PList # plam (\(x :: Term _ PInteger) -> x) # pnil) #== pnil) (pcon PTrue)
            ]
        , goldenGroup
            "pfilter"
            [ goldenAssertEqual "evens" ((pfilter # plam (\x -> pmod # x # 2 #== 0) # xs10) #== integerList [2, 4, 6, 8, 10]) (pcon PTrue)
            , goldenAssertEqual "gt5" ((pfilter # plam (5 #<) # xs10) #== integerList [6 .. 10]) (pcon PTrue)
            ]
        , goldenGroup
            "pzipWith"
            [ goldenAssertEqual "double" ((pzipWith' (+) # xs10 # xs10) #== integerList (fmap (* 2) [1 .. 10])) (pcon PTrue)
            ]
        , goldenGroup
            "pfoldl"
            [ goldenAssertEqual "nonempty" ((pfoldl # plam (-) # 0 # xs10) #== pconstant (foldl (-) 0 ([1 .. 10] :: [Integer]))) (pcon PTrue)
            , goldenAssertEqual "nonempty-primed" ((pfoldl' (-) # 0 # xs10) #== pconstant (foldl (-) 0 ([1 .. 10] :: [Integer]))) (pcon PTrue)
            , goldenAssertEqual "empty" ((pfoldl # plam (-) # 0 # integerList []) #== pconstant 0) (pcon PTrue)
            , goldenAssertEqual "empty-primed" ((pfoldl' (-) # 0 # integerList []) #== pconstant 0) (pcon PTrue)
            ]
        , goldenGroup
            "elemAt"
            [ goldenEval "elemAt_3_[1..10]" (pelemAt # 3 # integerList [1 .. 10])
            , goldenEval "elemAt_0_[1..10]" (pelemAt # 0 # integerList [1 .. 10])
            , goldenEval "elemAt_9_[1..10]" (pelemAt # 9 # integerList [1 .. 10])
            ]
        , goldenGroup
            "find"
            [ goldenEval "find_(==3)_[1..4]" (pfind # plam (#== 3) #$ integerList [1 .. 4])
            , goldenEval "find_(==5)_[1..4]" (pfind # plam (#== 5) #$ integerList [1 .. 4])
            ]
        , goldenGroup
            "x1+x2"
            [ goldenEval "builtin" ((phead #$ ptail # numList) + (phead # numList))
            , goldenEval "pmatch" $
                pmatch numList $ \case
                  PNil -> perror
                  PCons x xs ->
                    pmatch xs $ \case
                      PNil -> perror
                      PCons y _ -> x + y
            ]
        , goldenGroup
            "uncons"
            [ goldenEval "ChooseList" $
                pmatch numList $ \case
                  PNil -> perror
                  PCons _x xs -> xs
            , goldenEval "head-and-tail" $
                plet (phead # numList) $
                  \_x -> ptail # numList
            , goldenEval "head-and-tail-and-null" $
                plet (pnull # numList) $ \isEmpty ->
                  pmatch isEmpty $ \case
                    PTrue -> perror
                    PFalse -> plet (phead # numList) $ \_x -> ptail # numList
            ]
        ]
    , testGroup
        "Properties"
        [ testProperty "pfindEquiv" . forAllShrinkShow arbitrary shrink show $
            \(lst :: [Integer]) -> find even lst === plift (pmaybeToMaybeData #$ pfind # peven # pconstant lst)
        ]
    ]

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

xs10 :: Term _ (PList PInteger)
xs10 = integerList [1 .. 10]

numList :: Term _ (PBuiltinList PInteger)
numList = pconstant [1 .. 5]
