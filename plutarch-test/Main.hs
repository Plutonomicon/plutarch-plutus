{-# LANGUAGE CPP #-}

module Main (main) where

import qualified BaseSpec
import qualified ExtraSpec

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified Plutarch.FieldSpec as FieldSpec
import qualified Plutarch.MonadicSpec as MonadicSpec
#else
import qualified Plutarch.FFISpec as FFISpec
#endif

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Syd (describe, sydTest)

main :: IO ()
main = do
  setLocaleEncoding utf8
  sydTest BaseSpec.spec
  putStrLn "\n\n\n--------------------------------------------\n\n\n"
  sydTest ExtraSpec.spec

  putStrLn "\n\n\n--------------------------------------------\n\n\n"
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  sydTest $ describe "field" FieldSpec.spec
  sydTest $ describe "monadic" MonadicSpec.spec
#else
  sydTest $ describe "FFI" FFISpec.spec
#endif
