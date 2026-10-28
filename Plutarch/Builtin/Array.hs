module Plutarch.Builtin.Array (
  PArray (PArray),
  plengthOfArray,
  plistToArray,
  pindexArray,
  pmultiIndexArray,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (PBuiltinList)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Term (
  S,
  Term,
  pforce,
  phoistAcyclic,
  punsafeBuiltin,
  (:-->),
 )
import PlutusCore qualified as PLC

{- | A packed collection of values.

@since 1.11.0
-}
newtype PArray (a :: S -> Type) (s :: S)
  = PArray (Term s (PArray a))
  deriving stock
    ( -- | @since 1.11.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.11.0
      SOP.Generic
    )

{- | Get the length of an array, as per
[CIP-138](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0138).

@since 1.11.0
-}
plengthOfArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a :--> PInteger)
plengthOfArray = phoistAcyclic $ pforce $ punsafeBuiltin PLC.LengthOfArray

{- | Convert a (builtin) list to an array with the same contents in the same
order, as per
[CIP-138](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0138).

@since 1.11.0
-}
plistToArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBuiltinList a :--> PArray a)
plistToArray = phoistAcyclic $ pforce $ punsafeBuiltin PLC.ListToArray

{- | Index an array, as per
[CIP-138](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0138).

@since 1.11.0
-}
pindexArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a :--> PInteger :--> a)
pindexArray = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IndexArray

{- | Multi-index an array, as per
[CIP-156](https://cips.cardano.org/cip/CIP-0156).

@since 1.15.0
-}
pmultiIndexArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a :--> PBuiltinList PInteger :--> PBuiltinList a)
pmultiIndexArray = phoistAcyclic $ pforce $ punsafeBuiltin PLC.MultiIndexArray
