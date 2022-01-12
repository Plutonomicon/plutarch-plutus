{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Benchmark (NamedBenchmark, ScriptSizeBytes (ScriptSizeBytes))
import Benchmark qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.List qualified as List
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Integer
import Plutarch.List qualified as List
import Plutus.V1.Ledger.Api
import Text.PrettyPrint.Boxes qualified as B

main :: IO ()
main = do
  benchMain $
    benchGroup
      "simple"
      [ benchGroup
          "int"
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
      , benchGroup "bool" $
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
      , benchGroup
          "builtin:list"
          $ let numList = pconstant @(PBuiltinList PInteger) [1 .. 5]
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
      ]

benchGroup :: String -> [[NamedBenchmark]] -> [NamedBenchmark]
benchGroup groupName bs =
  [B.NamedBenchmark (groupName ++ ":" ++ name, benchmark) | B.NamedBenchmark (name, benchmark) <- concat bs]

bench :: String -> ClosedTerm a -> [NamedBenchmark]
bench name prog =
  [B.benchmarkScript name $ compile prog]

benchMain :: [NamedBenchmark] -> IO ()
benchMain benchmarks = do
  let csv = Csv.encodeDefaultOrderedByName benchmarks
  BSL.writeFile "bench.csv" csv
  putStrLn "Wrote to bench.csv:"
  putStrLn $ B.render $ renderNamedBudgets benchmarks
  where
    renderNamedBudgets :: [B.NamedBenchmark] -> B.Box
    renderNamedBudgets bs =
      let cols =
            List.transpose $
              [ [name, show cpu <> "(cpu)", show mem <> "(mem)", show sz <> "(bytes)"]
              | B.NamedBenchmark (name, B.Benchmark (ExCPU cpu) (ExMemory mem) (ScriptSizeBytes sz)) <- bs
              ]
       in B.hsep 2 B.left . map (B.vcat B.left . map B.text) $ cols
