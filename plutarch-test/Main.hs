{-# LANGUAGE CPP #-}
module Main (main) where

import qualified BaseSpec
import qualified ExtraSpec

#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified Plutarch.FFISpec as FFISpec
#endif

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Syd (sydTest)

main :: IO ()
main = do
  setLocaleEncoding utf8
  sydTest BaseSpec.spec
  putStrLn "\n\n\n--------------------------------------------\n\n\n"
  sydTest ExtraSpec.spec

#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  putStrLn "\n\n\n--------------------------------------------\n\n\n"
  sydTest FFISpec.spec
#endif
