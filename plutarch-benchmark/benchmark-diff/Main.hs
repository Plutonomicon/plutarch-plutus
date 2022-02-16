module Main (main) where

import Plutarch.Benchmark (decodeBenchmarks, diffBenchmarks, renderDiffTable)

import qualified Data.ByteString.Lazy as BSL
import Options.Applicative (
  argument,
  execParser,
  fullDesc,
  helper,
  info,
  liftA2,
  metavar,
  progDesc,
  str,
  (<**>),
 )
import qualified Text.PrettyPrint.Boxes as B

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let cmdOptions =
        liftA2
          (,)
          (argument str (metavar "BEFORE"))
          (argument str (metavar "AFTER"))
      opts =
        info
          (cmdOptions <**> helper)
          ( fullDesc
              <> progDesc "Diff two benchmark CSVs"
          )
  (oldPath, newPath) <- execParser opts
  Right old <- decodeBenchmarks <$> BSL.readFile oldPath
  Right new <- decodeBenchmarks <$> BSL.readFile newPath
  putStrLn . B.render . renderDiffTable $ diffBenchmarks old new
