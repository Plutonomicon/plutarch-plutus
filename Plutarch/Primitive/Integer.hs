module Plutarch.Primitive.Integer (
  PInteger,
  paddInteger,
  psubtractInteger,
  pmultiplyInteger,
  pdivideInteger,
  pquotientInteger,
  premainderInteger,
  pmodInteger,
) where

import Plutarch.Backend.Term (S, Term, punsafeBuiltin, (:-->))
import Plutarch.Primitive.Representation (PRepresentation)
import PlutusCore qualified as PLC

-- | @since wip
data PInteger (s :: S)

-- | @since wip
type instance PRepresentation PInteger = PInteger

-- | @since wip
paddInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
paddInteger = punsafeBuiltin PLC.AddInteger

-- | @since wip
psubtractInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
psubtractInteger = punsafeBuiltin PLC.SubtractInteger

-- | @since wip
pmultiplyInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmultiplyInteger = punsafeBuiltin PLC.MultiplyInteger

-- | @since wip
pdivideInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdivideInteger = punsafeBuiltin PLC.DivideInteger

-- | @since wip
pquotientInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pquotientInteger = punsafeBuiltin PLC.QuotientInteger

-- | @since wip
premainderInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
premainderInteger = punsafeBuiltin PLC.RemainderInteger

-- | @since wip
pmodInteger :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmodInteger = punsafeBuiltin PLC.ModInteger
