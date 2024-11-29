module Plutarch.Pair (PPair (..)) where

import GHC.Generics (Generic)
import Plutarch.Builtin.Bool ((#&&))
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType, pmatch)
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Internal.Term (PType, S, Term)
import Plutarch.Show (PShow)

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType (PPair a b) where type DPTStrat _ = PlutusTypeScott

instance (PEq a, PEq b) => PEq (PPair a b) where
  a #== b =
    pmatch a $ \(PPair ax ay) ->
      pmatch b $ \(PPair bx by) -> ax #== bx #&& ay #== by
