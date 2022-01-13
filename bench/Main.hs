module Main (main) where

import Benchmark
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Integer
import qualified Plutarch.List as List

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks =
  benchGroup
    "types"
    [ benchGroup "int" integerBench
    , benchGroup "bool" boolBench
    , benchGroup "builtin:intlist" intListBench
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

intListBench :: [[NamedBenchmark]]
intListBench =
  let numList = pconstant @(PBuiltinList PInteger) [1 .. 5]
   in [ bench "phead" $ List.phead # numList
      , bench "ptail" $ List.ptail # numList
      , -- Accessing the first two elements, and adds them.
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
      , -- Various ways to uncons a list
        benchGroup
          "uncons"
          [ -- ChooseList builtin, like uncons but fails on null lists
            bench "ChooseList" $
              pmatch numList $ \case
                PNil -> perror
                PCons x _xs ->
                  x
          , -- Retrieving head and tail of a list
            bench "head-and-tail" $
              plet (List.phead # numList) $ \_x ->
                List.ptail # numList
          , -- Retrieve head and tail using builtins, but fail on null lists.
            bench "head-and-tail-and-null" $
              plet (List.pnull # numList) $ \isEmpty ->
                pmatch' isEmpty $ \case
                  PTrue -> perror
                  PFalse -> plet (List.phead # numList) $ \_x ->
                    List.ptail # numList
          ]
      , bench
          "plength"
          $ List.plength # pconstant @(PBuiltinList PInteger) [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
      , bench
          "pelem"
          $ List.pelem # 1 # pconstant @(PBuiltinList PInteger) [5, 2, 3, 4, 7, 5, 1, 6, 2]
      , bench
          "pall"
          $ List.pall @PBuiltinList @PInteger # plam (const $ pconstant @PBool False) # pconstant [1, 2, 3, 4, 5, 6]
      , benchGroup
          "plistEquals"
          [ bench "==(n=3)" $ List.plistEquals @PBuiltinList @PInteger # pconstant [1, 2, 3] # pconstant [1, 2, 3]
          , bench "/=(n=4)" $ List.plistEquals @PBuiltinList @PInteger # pconstant [1, 2, 3, 4] # pconstant [1, 2, 3]
          , bench "/=(empty;n=3)" $ List.plistEquals @PBuiltinList @PInteger # pconstant [] # pconstant [1, 2, 3]
          ]
      ]
