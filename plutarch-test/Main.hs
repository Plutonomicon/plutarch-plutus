{-# LANGUAGE CPP #-}

module Main (main) where

import qualified BaseSpec
import qualified ExtraSpec

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import qualified Plutarch.FieldSpec as FieldSpec
import qualified Plutarch.MonadicSpec as MonadicSpec
import Plutarch.Test.Run (noUnusedGoldens)
#else
import qualified Plutarch.FFISpec as FFISpec
#endif

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hspec spec

-- We test for unused goldens, but do so only in GHC 9. Because, under GHC 8
-- certain modules are disabled (see the CPP below) which leads to legitimately
-- unused goldens detected leading to false positive in test failure.
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  noUnusedGoldens spec
#endif

spec :: Spec
spec = do
  BaseSpec.spec
  ExtraSpec.spec
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  describe "GHC-9-only" $ do
    FieldSpec.spec
    MonadicSpec.spec
#else
  describe "GHC-8-only" $ do
    FFISpec.spec
#endif
