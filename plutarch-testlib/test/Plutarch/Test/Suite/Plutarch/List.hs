module Plutarch.Test.Suite.Plutarch.List (tests, integerList) where

import Data.List (find)
import Plutarch.Internal.ListLike (pconvertLists, pfoldl')
import Plutarch.LedgerApi.Utils (pmaybeToMaybeData)
import Plutarch.List (pcheckSorted, preverse)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalFail, goldenGroup, plutarchGolden)
import Plutarch.Test.QuickCheck (checkHaskellEquivalent)
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
        , goldenEval "phead" (1 #== (phead # xs10))
        , goldenEval "ptail" (integerList [2 .. 10] #== ptail # xs10)
        , goldenGroup
            "pnull"
            [ goldenEval "empty" (pnull # integerList [])
            , goldenEval "nonempty" (pnot #$ pnull # xs10)
            ]
        , goldenGroup
            "pconcat"
            [ goldenEval "identity" ((pconcat # xs10 # pnil #== pconcat # pnil # xs10) #&& (pconcat # pnil # xs10 #== xs10))
            ]
        , goldenGroup
            "pmap"
            [ goldenEval "eg" ((pmap # plam (\x -> x + x) # xs10) #== integerList (fmap (* 2) [1 .. 10]))
            , goldenEval "identity" ((pmap @PList # plam (\(x :: Term _ PInteger) -> x) # pnil) #== pnil)
            ]
        , goldenGroup
            "pfilter"
            [ goldenEval "evens" ((pfilter # plam (\x -> pmod # x # 2 #== 0) # xs10) #== integerList [2, 4, 6, 8, 10])
            , goldenEval "gt5" ((pfilter # plam (5 #<) # xs10) #== integerList [6 .. 10])
            ]
        , goldenGroup
            "pzipWith"
            [ goldenEval "double" ((pzipWith' (+) # xs10 # xs10) #== integerList (fmap (* 2) [1 .. 10]))
            ]
        , goldenGroup
            "pfoldl"
            [ goldenEval "nonempty" ((pfoldl # plam (-) # 0 # xs10) #== pconstant (foldl (-) 0 ([1 .. 10] :: [Integer])))
            , goldenEval "nonempty-primed" ((pfoldl' (-) # 0 # xs10) #== pconstant (foldl (-) 0 ([1 .. 10] :: [Integer])))
            , goldenEval "empty" ((pfoldl # plam (-) # 0 # integerList []) #== pconstant 0)
            , goldenEval "empty-primed" ((pfoldl' (-) # 0 # integerList []) #== pconstant 0)
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
                (preverse # pconstant @(PBuiltinList PInteger) ([] :: [Integer]))
                (pconstant ([] :: [Integer]))
            , testEvalEqual
                "reversing a singleton is identity"
                (preverse # pconstant @(PBuiltinList PInteger) ([1] :: [Integer]))
                (pconstant ([1] :: [Integer]))
            , testEvalEqual
                "reversing a non-singleton"
                (preverse # pconstant @(PBuiltinList PInteger) ([1, 2] :: [Integer]))
                (pconstant ([2, 1] :: [Integer]))
            ]
        , testGroup
            "pcheckSorted"
            [ testEvalEqual
                "empty lists are sorted"
                (pcheckSorted # pconstant @(PBuiltinList PInteger) ([] :: [Integer]))
                (pcon PTrue)
            , testEvalEqual
                "singleton lists are sorted"
                (pcheckSorted # pconstant @(PBuiltinList PInteger) ([1] :: [Integer]))
                (pcon PTrue)
            , testEvalEqual
                "two items in the right order are sorted"
                (pcheckSorted # pconstant @(PBuiltinList PInteger) ([1, 2] :: [Integer]))
                (pcon PTrue)
            , testEvalEqual
                "two items in the wrong order are not sorted"
                (pcheckSorted # pconstant @(PBuiltinList PInteger) ([2, 1] :: [Integer]))
                (pcon PFalse)
            ]
        ]
    , testGroup
        "Properties"
        [ testProperty "find <-> pfind" $
            checkHaskellEquivalent @(PBuiltinList PInteger)
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
