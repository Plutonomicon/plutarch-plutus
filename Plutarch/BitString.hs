{-# LANGUAGE UndecidableInstances #-}

module Plutarch.BitString (
  -- * Type
  PBitString (..),

  -- * Functions
  preadBit,
  pwriteBits,
  pshift,
  protate,
  pcountSetBits,
  pfindFirstSetBit,
  pfindFirstSetBit',
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data (PBuiltinList)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.Lift (
  DeriveNewtypePLiftable,
  PLiftable,
  PLifted (PLifted),
 )
import Plutarch.Internal.Numeric (pzero)
import Plutarch.Internal.Ord (POrd ((#<)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType,
  pcon,
 )
import Plutarch.Internal.Semigroup (PMonoid, PSemigroup)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plet,
  punsafeBuiltin,
  (#),
  (:-->),
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Repr.Newtype (DeriveNewtypePlutusType (DeriveNewtypePlutusType))
import PlutusCore qualified as PLC

{- | A wrapper around 'PByteString' for CIP-122 and CIP-123 bitwise operations.

= Note

This type exists because /bit/ and /byte/ indexes work in different
directions. To avoid confusing behaviour, we require an explicit wrapping of
'PByteString's to use bitwise functionality: this way, it's clear where which
scheme applies.

@since 1.10.0
-}
newtype PBitString (s :: S) = PBitString (Term s PByteString)
  deriving stock
    ( -- | @since 1.10.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.10.0
      SOP.Generic
    , -- | @since 1.10.0
      PEq
    , -- | @since 1.10.0
      POrd
    , -- | @since 1.10.0
      PSemigroup
    , -- | @since 1.10.0
      PMonoid
    )
  deriving
    ( -- | @since 1.10.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PBitString)

-- | @since 1.10.0
deriving via
  DeriveNewtypePLiftable PBitString ByteString
  instance
    PLiftable PBitString

{- | Bit access operation, as defined in [CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#readbit).

@since 1.10.0
-}
preadBit :: forall (s :: S). Term s (PBitString :--> PInteger :--> PBool)
preadBit = punsafeBuiltin PLC.ReadBit

{- | Given a list of positions, set the bits at those positions.

This works similarly to the @writeBits@ operation in
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#writebits)
with regard to the list of indexes. Effectively, @psetBits b ixes@ is
equivalent to @writeBits b . map (, True) $ ixes@. All caveats that this
entails from the CIP-122 description apply.

@since 1.10.0
-}

{- | Sets bits, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#writebits).

@since 1.10.0
-}
pwriteBits :: forall (s :: S). Term s (PBitString :--> PBuiltinList PInteger :--> PBool :--> PBitString)
pwriteBits = punsafeBuiltin PLC.WriteBits

{- | Performs a shift, as per
[CIP-123](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0123/README.md#bitwiseshift).

@since 1.10.0
-}
pshift :: forall (s :: S). Term s (PBitString :--> PInteger :--> PBitString)
pshift = punsafeBuiltin PLC.ShiftByteString

{- | Performs a rotation, as per
[CIP-123](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0123/README.md#bitwiserotate).

@since 1.10.0
-}
protate :: forall (s :: S). Term s (PBitString :--> PInteger :--> PBitString)
protate = punsafeBuiltin PLC.RotateByteString

{- | Counts the number of set bits, as per
[CIP-123](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0123/README.md#countsetbits).

@since 1.10.0
-}
pcountSetBits :: forall (s :: S). Term s (PBitString :--> PInteger)
pcountSetBits = punsafeBuiltin PLC.CountSetBits

{- | Finds the index of the first set bit, as per
[CIP-123](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0123/README.md#findfirstsetbit).

= Note

This returns @-1@ if the argument is either empty, or contains no set bits.

@since 1.10.0
-}
pfindFirstSetBit' :: forall (s :: S). Term s (PBitString :--> PInteger)
pfindFirstSetBit' = punsafeBuiltin PLC.FindFirstSetBit

{- | As @pfindFirstSetBit'@, but produces 'PNothing' if the argument is empty,
or contains no set bits.

@since 1.10.0
-}
pfindFirstSetBit :: forall (s :: S). Term s (PBitString :--> PMaybe PInteger)
pfindFirstSetBit = phoistAcyclic $ plam $ \bs ->
  plet (punsafeBuiltin PLC.FindFirstSetBit # pto bs) $ \result ->
    pif
      (result #< pzero)
      (pcon PNothing)
      (pcon . PJust $ result)
