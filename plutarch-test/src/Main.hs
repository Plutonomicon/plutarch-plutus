module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec (spec)
import Test.Syd (sydTest)

main :: IO ()
main = do
  setLocaleEncoding utf8
  sydTest spec
