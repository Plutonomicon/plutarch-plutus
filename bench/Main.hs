{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Benchmark (NamedBenchmark, ScriptSizeBytes (ScriptSizeBytes))
import Benchmark qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.List qualified as List
import Plutarch
import Plutarch.Integer
import Plutus.V1.Ledger.Api
import Text.PrettyPrint.Boxes qualified as B

main :: IO ()
main = do
  benchMain $
    benchGroup
      "simple"
      [ benchGroup
          "add(twice-used)"
          [ bench "inlined" $ addInlined 12 32 + addInlined 5 4
          , bench "unhoist" $ addUnhoisted # 12 # 32 + addUnhoisted # 5 # 4
          , bench "hoisted" $ addHoisted # 12 # 32 + addHoisted # 5 # 4
          ]
      ]

addInlined :: Term s PInteger -> Term s PInteger -> Term s PInteger
addInlined x y = x + y + 1

addUnhoisted :: Term s (PInteger :--> PInteger :--> PInteger)
addUnhoisted = plam $ \x y -> x + y + 1

addHoisted :: Term s (PInteger :--> PInteger :--> PInteger)
addHoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

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
