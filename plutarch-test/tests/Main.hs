{-# LANGUAGE CPP #-}

module Main (main) where

import Spec qualified

import Test.Hspec (Spec, hspec)

import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
  setLocaleEncoding utf8

  -- FIXME: Re-enable unused golden checks
  -- Old:
  -- We test for unused goldens, but do so only in GHC 9. Because, under GHC 8
  -- certain modules are disabled (see the CPP below) which leads to legitimately
  -- unused goldens detected leading to false positive in test failure.
  --  noUnusedGoldens =<< hspecAndReturnForest spec
  hspec spec

spec :: Spec
spec = Spec.spec
