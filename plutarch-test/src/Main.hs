module Main (main) where

import Spec (spec)
import Test.Syd (sydTest)

main :: IO ()
main = sydTest spec
