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
  pintegerToByteString,
  pintegerToByteStringSized,
) where

import GHC.Generics (Generic)
import Plutarch.Internal.Builtin (
  PBool (PFalse, PTrue),
  PByteString,
  PInteger,
  plam,
  pto,
 )
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd, PPartialOrd)
import Plutarch.Internal.PlutusType (
  DPTStrat,
  DerivePlutusType,
  PlutusType,
  pcon,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  punsafeBuiltin,
  (#),
  (:-->),
 )
import Plutarch.Positive (PPositive)
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
  Term s PEndianness ->
  Term s (PByteString :--> PInteger)
pbyteStringToInteger e = plam $ \bs ->
  punsafeBuiltin PLC.ByteStringToInteger # pto e # bs

{- | Convert a (non-negative) 'PInteger' into a 'PByteString'. This will produce
a result of the minimal size required: if you want to specify a size, use
'pintegerToByteStringSized'. For details, see
[CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#builtinintegertobytestring).

= Note

This conversion is unsafe, as it will error when given a non-negative
integer.
-}
pintegerToByteString ::
  forall (s :: S).
  Term s PEndianness ->
  Term s (PInteger :--> PByteString)
pintegerToByteString e = plam $ \i ->
  punsafeBuiltin PLC.IntegerToByteString # pto e # (0 :: Term s PInteger) # i

{- | As 'pintegerToByteString', but allows specifying a required size. If
a size larger than the minimum is specified, the result will be padded with zero
bytes, positioned according to the endianness argument.

For more details, see [CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121#builtinintegertobytestring).

= Note

This conversion is unsafe. In addition to the reasons for
'punsafeIntegerToByteString' being unsafe, this will also error if the
requested size is too large (currently 8192 is the limit) or too small to fit
the specified 'PInteger'.
-}
pintegerToByteStringSized ::
  forall (s :: S).
  Term s PEndianness ->
  Term s (PPositive :--> PInteger :--> PByteString)
pintegerToByteStringSized e = plam \len i ->
  punsafeBuiltin PLC.IntegerToByteString # pto e # pto len # i