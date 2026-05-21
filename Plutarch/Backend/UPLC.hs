{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Backend.UPLC (
  UPLCTerm (..),
) where

import UntypedPlutusCore qualified as UPLC

newtype UPLCTerm = UPLCTerm (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
  deriving (Eq, Show) via (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
