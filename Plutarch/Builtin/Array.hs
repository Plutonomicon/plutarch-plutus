module Plutarch.Builtin.Array (
  PArray (PArray),
  plengthOfArray,
  plistToArray,
  pindexArray,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (PBuiltinList)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Term (
  S,
  Term,
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
plengthOfArray = punsafeBuiltin PLC.LengthOfArray

{- | Convert a (builtin) list to an array with the same contents in the same
order, as per
[CIP-138](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0138).

@since 1.11.0
-}
plistToArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBuiltinList a :--> PArray a)
plistToArray = punsafeBuiltin PLC.ListToArray

{- | Index an array, as per
[CIP-138](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0138).

@since 1.11.0
-}
pindexArray ::
  forall (a :: S -> Type) (s :: S).
  Term s (PArray a :--> PInteger :--> a)
pindexArray = punsafeBuiltin PLC.IndexArray
