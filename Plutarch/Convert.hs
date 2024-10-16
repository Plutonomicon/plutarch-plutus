{- | Conversions between various types.

@since WIP
-}
module Plutarch.Convert (
  -- * Types
  PEndianness,

  -- * Functions
  pmostSignificantFirst,
  pmostSignificantLast,
  pbyteStringToInteger,
  punsafeIntegerToByteString,
  punsafeIntegerToByteStringSized,
) where

import GHC.Generics (Generic)
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, POrd, PPartialOrd)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (S, Term, (#), (:-->))
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DPTStrat,
  DerivePlutusType,
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Positive (PPositive)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

{- | Type designating whether a conversion should be most-significant-first or
most-significant-last. See
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#representation)
for more details on this.

@since WIP
-}
newtype PEndianness (s :: S) = PEndianness (Term s PBool)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    , -- | @since WIP
      PEq
    , -- | @since WIP
      PPartialOrd
    , -- | @since WIP
      POrd
    )

-- | @since WIP
instance DerivePlutusType PEndianness where
  type DPTStrat _ = PlutusTypeNewtype

{- | Indicates the conversion should be most-significant-first.

@since WIP
-}
pmostSignificantFirst :: forall (s :: S). Term s PEndianness
pmostSignificantFirst = pcon . PEndianness . pcon $ PTrue

{- | Indicates the conversion should be most-significant-last.

@since WIP
-}
pmostSignificantLast :: forall (s :: S). Term s PEndianness
pmostSignificantLast = pcon . PEndianness . pcon $ PFalse

{- | Convert a 'PByteString' into a 'PInteger', as per
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#builtinbytestringtointeger).

@since WIP
-}
pbyteStringToInteger ::
  forall (s :: S).
  Term s (PEndianness :--> PByteString :--> PInteger)
pbyteStringToInteger = plam $ \e bs -> pmatch e $ \(PEndianness e') ->
  punsafeBuiltin PLC.ByteStringToInteger # e' # bs

{- | Convert a (non-negative) 'PInteger' into a 'PByteString'. This will produce
a result of the minimal size required: if you want to specify a size, use
'punsafeIntegerToByteStringSized'. For details, see
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#builtinintegertobytestring).

= Note

This conversion is unsafe, as it will error when given a non-negative
integer.
-}
punsafeIntegerToByteString ::
  forall (s :: S).
  Term s (PEndianness :--> PInteger :--> PByteString)
punsafeIntegerToByteString = plam $ \e i -> pmatch e $ \(PEndianness e') ->
  punsafeBuiltin PLC.IntegerToByteString # e' # (0 :: Term s PInteger) # i

{- | As 'punsafeIntegerToByteString', but allows specifying a required size.

= Note

This conversion is unsafe. In addition to the reasons for
'punsafeIntegerToByteString' being unsafe, this will also error if the
requested size is too large (currently 8192 is the limit).
-}
punsafeIntegerToByteStringSized ::
  forall (s :: S).
  Term s (PEndianness :--> PPositive :--> PInteger :--> PByteString)
punsafeIntegerToByteStringSized = plam $ \e len i -> pmatch e $ \(PEndianness e') ->
  punsafeBuiltin PLC.IntegerToByteString # e' # pto len # i
