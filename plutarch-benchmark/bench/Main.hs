module Main (main) where

import Plutarch.Benchmark (NamedBenchmark, benchMain)

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks = []