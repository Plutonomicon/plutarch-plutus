{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Benchmark (NamedBenchmark)
import Benchmark qualified as B
import Control.Monad.Writer (execWriterT, tell)
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.List qualified as List
import Plutarch
import Plutarch.Integer
import Text.PrettyPrint.Boxes qualified as B

main :: IO ()
main = do
  -- TODO: Benchmark examples directly from the examples package.
  benchmarks <- benchAdd
  -- Report final benchmark results to stdout and as CSV file.
  let csv = Csv.encodeDefaultOrderedByName benchmarks
  BSL.writeFile "bench.csv" csv
  putStrLn "Wrote to bench.csv:"
  putStrLn $ B.render $ renderNamedBudgets benchmarks
  where
    renderNamedBudgets :: [B.NamedBenchmark] -> B.Box
    renderNamedBudgets bs =
      let cols = List.transpose $ [[name, show cpu, show mem, show sz] | B.NamedBenchmark (name, B.Benchmark cpu mem sz) <- bs]
       in B.hsep 2 B.left . map (B.vcat B.left . map B.text) $ cols

addInlined :: Term s PInteger -> Term s PInteger -> Term s PInteger
addInlined x y = x + y + 1

addUnhoisted :: Term s (PInteger :--> PInteger :--> PInteger)
addUnhoisted = plam $ \x y -> x + y + 1

addHoisted :: Term s (PInteger :--> PInteger :--> PInteger)
addHoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

benchAdd :: IO [NamedBenchmark]
benchAdd = do
  execWriterT $ do
    let k = "add(used-twice):"
        one = (: [])
    tell . one $ B.benchmarkScript (k <> "inlined") $ compile $ addInlined 12 32 + addInlined 5 4
    tell . one $ B.benchmarkScript (k <> "unhoist") $ compile $ addUnhoisted # 12 # 32 + addUnhoisted # 5 # 4
    tell . one $ B.benchmarkScript (k <> "hoisted") $ compile $ addHoisted # 12 # 32 + addHoisted # 5 # 4
