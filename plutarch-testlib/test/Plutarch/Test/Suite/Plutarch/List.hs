module Plutarch.Test.Suite.Plutarch.List (tests, integerList) where

import Data.List (find)
import Plutarch.LedgerApi.Utils (pmaybeToMaybeData)
import Plutarch.List (pcheckSorted, pconvertLists, pfoldl', preverse)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalEqual, goldenEvalFail, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkHaskellEquivalent)
import Plutarch.Test.Unit (testEvalEqual)
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
        [ goldenEvalFail "pmatch" (pmatch (integerList [1, 3, 1]) (const perror))
        , goldenEvalEqual "phead" (1 #== (phead # xs10)) (pcon PTrue)
        , goldenEvalEqual "ptail" (integerList [2 .. 10] #== ptail # xs10) (pcon PTrue)
        , goldenGroup
            "pnull"
            [ goldenEvalEqual "empty" (pnull # integerList []) (pcon PTrue)
            , goldenEvalEqual "nonempty" (pnot #$ pnull # xs10) (pcon PTrue)
            ]
        , goldenGroup
            "pconcat"
            [ goldenEvalEqual "identity" ((pconcat # xs10 # pnil #== pconcat # pnil # xs10) #&& (pconcat # pnil # xs10 #== xs10)) (pcon PTrue)
            ]
        , goldenGroup
            "pmap"
            [ goldenEvalEqual "eg" ((pmap # plam (\x -> x + x) # xs10) #== integerList (fmap (* 2) [1 .. 10])) (pcon PTrue)
            , goldenEvalEqual "identity" ((pmap @PList # plam (\(x :: Term _ PInteger) -> x) # pnil) #== pnil) (pcon PTrue)
            ]
        , goldenGroup
            "pfilter"
            [ goldenEvalEqual "evens" ((pfilter # plam (\x -> pmod # x # 2 #== 0) # xs10) #== integerList [2, 4, 6, 8, 10]) (pcon PTrue)
            , goldenEvalEqual "gt5" ((pfilter # plam (5 #<) # xs10) #== integerList [6 .. 10]) (pcon PTrue)
            ]
        , goldenGroup
            "pzipWith"
            [ goldenEvalEqual "double" ((pzipWith' (+) # xs10 # xs10) #== integerList (fmap (* 2) [1 .. 10])) (pcon PTrue)
            ]
        , goldenGroup
            "pfoldl"
            [ goldenEvalEqual "nonempty" ((pfoldl # plam (-) # 0 # xs10) #== pconstant (foldl (-) 0 ([1 .. 10] :: [Integer]))) (pcon PTrue)
            , goldenEvalEqual "nonempty-primed" ((pfoldl' (-) # 0 # xs10) #== pconstant (foldl (-) 0 ([1 .. 10] :: [Integer]))) (pcon PTrue)
            , goldenEvalEqual "empty" ((pfoldl # plam (-) # 0 # integerList []) #== pconstant 0) (pcon PTrue)
            , goldenEvalEqual "empty-primed" ((pfoldl' (-) # 0 # integerList []) #== pconstant 0) (pcon PTrue)
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
        "Unit tests"
        [ testGroup
            "preverse"
            [ testEvalEqual
                "reversing an empty list is identity"
                (preverse # pconstant ([] :: [Integer]))
                (pconstant ([] :: [Integer]))
            , testEvalEqual
                "reversing a singleton is identity"
                (preverse # pconstant ([1] :: [Integer]))
                (pconstant ([1] :: [Integer]))
            , testEvalEqual
                "reversing a non-singleton"
                (preverse # pconstant ([1, 2] :: [Integer]))
                (pconstant ([2, 1] :: [Integer]))
            ]
        , testGroup
            "pcheckSorted"
            [ testEvalEqual
                "empty lists are sorted"
                (pcheckSorted # pconstant ([] :: [Integer]))
                (pcon PTrue)
            , testEvalEqual
                "singleton lists are sorted"
                (pcheckSorted # pconstant ([1] :: [Integer]))
                (pcon PTrue)
            , testEvalEqual
                "two items in the right order are sorted"
                (pcheckSorted # pconstant ([1, 2] :: [Integer]))
                (pcon PTrue)
            , testEvalEqual
                "two items in the wrong order are not sorted"
                (pcheckSorted # pconstant ([2, 1] :: [Integer]))
                (pcon PFalse)
            ]
        ]
    , testGroup
        "Properties"
        [ testProperty "find <-> pfind" $
            checkHaskellEquivalent
              (find @[] @Integer even)
              (plam $ \lst -> pmaybeToMaybeData #$ pfind # peven # lst)
        ]
    ]

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

xs10 :: Term _ (PList PInteger)
xs10 = integerList [1 .. 10]

numList :: Term _ (PBuiltinList PInteger)
numList = pconstant [1 .. 5]
