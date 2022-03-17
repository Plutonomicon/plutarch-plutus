module Main (main) where

import qualified BaseSpec
import qualified ExtraSpec
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Syd (sydTest)

main :: IO ()
main = do
  setLocaleEncoding utf8
  sydTest BaseSpec.spec
  putStrLn "\n\n\n--------------------------------------------\n\n\n"
  sydTest ExtraSpec.spec
