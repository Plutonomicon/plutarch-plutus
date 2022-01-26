module Plutarch.Pair (PPair (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch (PType, PlutusType, S, Term, gpcon, gpmatch, pcon', pmatch')

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

instance PlutusType (PPair a b) where
  pcon' x = gpcon @(PPair a b) $ from x
  pmatch' x f = gpmatch @(PPair a b) x (f . to)
