{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Pair (PPair (..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.PlutusType (PlutusType)
import Plutarch.Internal.Show (PShow)
import Plutarch.Internal.Term (S, Term)
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PEq
    , -- | @since WIP
      PShow
    )

-- | @since WIP
deriving via
  DeriveAsSOPStruct (PPair a b)
  instance
    PlutusType (PPair a b)
