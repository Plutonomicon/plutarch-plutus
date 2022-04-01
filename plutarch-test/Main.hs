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
import qualified Hedgehog.Main as Hedgehog
import qualified Plutarch.ListSpec as ListSpec
import qualified Plutarch.ListUtilsSpec as ListUtilsSpec
import Plutarch.Test (TrailSpec)
import Plutarch.Test.Run (runPlutarchSpec)

main :: IO ()
main = do
  setLocaleEncoding utf8
  runPlutarchSpec spec
  -- TODO: Can these properties actually run with hspec (using hspec-hedgehog)?
  Hedgehog.defaultMain props

props :: [IO Bool]
props =
  [ ListSpec.props
  , ListUtilsSpec.props
  ]
spec :: TrailSpec
spec = do
  BaseSpec.spec
  ExtraSpec.spec
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  FieldSpec.spec
  MonadicSpec.spec
#else
  FFISpec.spec
#endif
