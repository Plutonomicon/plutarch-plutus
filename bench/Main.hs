{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Benchmark
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Integer
import Plutarch.List qualified as List

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks =
  benchGroup
    "types"
    [ benchGroup "int" integerBench
    , benchGroup "bool" boolBench
    , benchGroup "builtin:list" listBench
    ]

integerBench :: [[NamedBenchmark]]
integerBench =
  [ -- Calling add twice
    benchGroup
      "add(2)"
      $ let addInlined :: Term s PInteger -> Term s PInteger -> Term s PInteger
            addInlined x y = x + y + 1
            addUnhoisted :: Term s (PInteger :--> PInteger :--> PInteger)
            addUnhoisted = plam $ \x y -> x + y + 1
            addHoisted :: Term s (PInteger :--> PInteger :--> PInteger)
            addHoisted = phoistAcyclic $ plam $ \x y -> x + y + 1
         in [ bench "inlined" $ addInlined 12 32 + addInlined 5 4
            , bench "unhoist" $ addUnhoisted # 12 # 32 + addUnhoisted # 5 # 4
            , bench "hoisted" $ addHoisted # 12 # 32 + addHoisted # 5 # 4
            ]
  ]

boolBench :: [[NamedBenchmark]]
boolBench =
  let true = pconstant @PBool True
      false = pconstant @PBool False
      pandNoHoist = phoistAcyclic $ plam $ \x y -> pif' # x # y # (pdelay $ pcon PFalse)
   in [ benchGroup
          "and"
          [ bench "strict" $ pand' # true # false
          , bench "lazy" $ (#&&) true false
          , -- Calling `pand` twice.
            bench "pand(2)" $
              let x = pand # true # pdelay false
               in pand # true # x
          , bench "pand(2):unhoisted" $
              let x = pandNoHoist # true # pdelay false
               in pandNoHoist # true # x
          ]
      ]

listBench :: [[NamedBenchmark]]
listBench =
  let numList = pconstant @(PBuiltinList PInteger) [1 .. 5]
   in [ bench "phead" $ List.phead # numList
      , bench "ptail" $ List.ptail # numList
      , -- Accessing the first two elements
        benchGroup
          "x1+x2"
          [ -- Via HeadList and TailList only
            bench "builtin" $
              (List.phead #$ List.ptail # numList) + (List.phead # numList)
          , -- Via ChooseList (twice invoked)
            bench "pmatch" $
              pmatch numList $ \case
                PNil -> perror
                PCons x xs ->
                  pmatch xs $ \case
                    PNil -> perror
                    PCons y _ ->
                      x + y
          ]
      ]
