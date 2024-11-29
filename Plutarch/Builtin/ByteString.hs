{-# LANGUAGE FlexibleInstances #-}

module Plutarch.Builtin.ByteString where

import Plutarch.Builtin.Bool
import Plutarch.Builtin.Integer
import Plutarch.Builtin.Opaque
import Plutarch.Internal.Term

import {-# SOURCE #-} Plutarch.Internal.PLam

import Data.ByteString qualified as BS
import GHC.Generics (Generic)

import PlutusCore qualified as PLC

-- | Plutus 'BuiltinByteString'
newtype PByteString s = PByteString (Term s POpaque)
  deriving stock (Generic)

{- | A Plutarch-level representation of bytes.

= Note =

This type is intentionally quite restrictive, as it's not really meant to be
computed with. Instead, it ensures certain operations' type safety while also
allowing more sensible signatures. If you want to do anything with 'PByte's,
we recommend converting them to 'PInteger's first.

@since WIP
-}
newtype PByte (s :: S) = PByte (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )

instance Semigroup (Term s PByteString) where
  x <> y = punsafeBuiltin PLC.AppendByteString # x # y

instance Monoid (Term s PByteString) where
  mempty = punsafeConstantInternal $ PLC.someValue BS.empty

{- | Indicates that padding semantics should be used.

@since WIP
-}
ppadding :: forall (s :: S). Term s PLogicOpSemantics
ppadding = punsafeCoerce ptrue

{- | Indicates that truncation semantics should be used.

@since WIP
-}
ptruncation :: forall (s :: S). Term s PLogicOpSemantics
ptruncation = punsafeCoerce pfalse

{- | Type designating whether logical operations should use padding or
truncation semantics. See
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#padding-versus-truncation-semantics)
for more details on this.

@since WIP
-}
newtype PLogicOpSemantics (s :: S) = PLogicOpSemantics (Term s PBool)
  deriving stock (Generic)

{- | Perform the logical AND of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicaland).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
pandBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
pandBS = punsafeBuiltin PLC.AndByteString

{- | Perform the logical OR of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalor).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
porBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
porBS = punsafeBuiltin PLC.OrByteString

{- | Perform the logical XOR of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalxor).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
pxorBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
pxorBS = punsafeBuiltin PLC.XorByteString

{- | Perform the logical complement of a 'PByteString', as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalcomplement).

@since WIP
-}
pcomplementBS ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pcomplementBS = punsafeBuiltin PLC.ComplementByteString

{- | Construct a 'PByteString' of the specified length (0 if negative)
consisting entirely of zero bytes.

@since WIP
-}
pzeroesBS :: forall (s :: S). Term s (PInteger :--> PByteString)
pzeroesBS = punsafeBuiltin PLC.ReplicateByte

{- | Construct a 'PByteString' of the specified length (0 if negative)
consisting entirely of ones; that is, where every byte is @0xFF@.

@since WIP
-}
ponesBS :: forall (s :: S). Term s (PInteger :--> PByteString)
ponesBS = punsafeBuiltin PLC.ReplicateByte

{- | Given a desired length and a 'PByte', construct a 'PByteString' of the
specified length (0 if negative) consisting entirely of that 'PByte'.

@since WIP
-}
preplicateBS :: forall (s :: S). Term s (PInteger :--> PByte :--> PByteString)
preplicateBS = punsafeBuiltin PLC.ReplicateByte

{- | Prepend a 'PByte' to a 'PByteString.

@since WIP
-}
pconsBS :: Term s (PByte :--> PByteString :--> PByteString)
pconsBS = punsafeBuiltin PLC.ConsByteString

{- | Convert a 'PByte' into its corresponding 'PInteger'.

@since WIP
-}
pbyteToInteger :: Term s (PByte :--> PInteger)
pbyteToInteger = phoistAcyclic $ plam punsafeCoerce

{- | Try to convert a 'PInteger' into its corresponding 'PByte'. This operation
is checked, and will error if given a negative 'PInteger', or one too large
to fit into a byte.

@since WIP
-}
pintegerToByte :: Term s (PInteger :--> PByte)
pintegerToByte = phoistAcyclic $ plam punsafeCoerce

{- | Slice a 'PByteString' with given start index and slice length.

>>> (pslice # 2 # 3 phexByteStr "4102afde5b2a") #== phexByteStr "afde5b"
-}
psliceBS :: Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
psliceBS = punsafeBuiltin PLC.SliceByteString

-- | Find the length of a 'PByteString'.
plengthBS :: Term s (PByteString :--> PInteger)
plengthBS = punsafeBuiltin PLC.LengthOfByteString

{- | Given a valid index into a 'PByteString', returns the 'PByte' at that
index. Will crash if given an out-of-bounds index.

@since WIP
-}
pindexBS :: Term s (PByteString :--> PInteger :--> PByte)
pindexBS = punsafeBuiltin PLC.IndexByteString
