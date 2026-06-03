{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Backend.UPLC (
  UPLCTerm (..),
) where

import Data.Hashable (Hashable)
import UntypedPlutusCore qualified as UPLC

newtype UPLCTerm = UPLCTerm (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
  deriving (Eq, Show, Hashable) via (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
