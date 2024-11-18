module Plutarch.Builtin.Data (
  -- * Type
  PData (..),

  -- * Builtins
  pbuiltinChooseData,
  pbuiltinEqualsData,
  pbuiltinSerialiseData,
  pbuiltinIData,
  pbuiltinUnIData,
  pbuiltinBData,
  pbuiltinUnBData,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Term (
  PDelayed,
  S,
  Term,
  punsafeBuiltin,
  (:-->),
 )
import PlutusCore qualified as PLC

-- | @since WIP
newtype PData (s :: S) = PData (Term s PData)

-- | @since WIP
pbuiltinChooseData ::
  forall (a :: S -> Type) (s :: S).
  Term s (PData :--> a :--> a :--> a :--> a :--> a :--> PDelayed a)
pbuiltinChooseData = punsafeBuiltin PLC.ChooseData

-- | @since WIP
pbuiltinEqualsData ::
  forall (s :: S).
  Term s (PData :--> PData :--> PBool)
pbuiltinEqualsData = punsafeBuiltin PLC.EqualsData

-- | @since WIP
pbuiltinSerialiseData ::
  forall (s :: S).
  Term s (PData :--> PByteString)
pbuiltinSerialiseData = punsafeBuiltin PLC.SerialiseData

-- | @since WIP
pbuiltinIData ::
  forall (s :: S).
  Term s (PInteger :--> PData)
pbuiltinIData = punsafeBuiltin PLC.IData

-- | @since WIP
pbuiltinUnIData ::
  forall (s :: S).
  Term s (PData :--> PInteger)
pbuiltinUnIData = punsafeBuiltin PLC.UnIData

-- | @since WIP
pbuiltinBData ::
  forall (s :: S).
  Term s (PByteString :--> PData)
pbuiltinBData = punsafeBuiltin PLC.BData

-- | @since WIP
pbuiltinUnBData ::
  forall (s :: S).
  Term s (PData :--> PByteString)
pbuiltinUnBData = punsafeBuiltin PLC.UnBData
