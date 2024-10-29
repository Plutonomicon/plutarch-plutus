{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.ByteString (
  -- * Types
  PByte,
  PLogicOpSemantics,

  -- * Functions

  -- ** Construction
  ppadding,
  ptruncation,
  pzeroesBS,
  ponesBS,
  preplicateBS,

  -- ** Byte-oriented
  pallBS,
  pandBS,
  porBS,
  pxorBS,
  pcomplementBS,
  pconsBS,
  psliceBS,
  plengthBS,
  pindexBS,

  -- ** Conversion
  pbyteToInteger,
  pintegerToByte,
  punsafeIntegerToByte,

  -- ** Other
  phexByteStr,
) where

import Data.Bits (toIntegralSized)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Plutarch.Integer ()
import Plutarch.Internal.Builtin (
  PBool (PFalse, PTrue),
  PByteString,
  PInteger,
  POpaque,
  pfix,
  pif,
  plam,
  (#||),
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd, PPartialOrd ((#<), (#<=)))
import Plutarch.Internal.PlutusType (
  DPTStrat,
  DerivePlutusType,
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Lift (
  PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC

instance Semigroup (Term s PByteString) where
  x <> y = punsafeBuiltin PLC.AppendByteString # x # y

instance Monoid (Term s PByteString) where
  mempty = pconstant BS.empty

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
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PByte where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PByte where
  type PLifted PByte = Word8

-- | @since WIP
instance PConstantDecl Word8 where
  type PConstantRepr Word8 = Integer
  type PConstanted Word8 = PByte
  {-# INLINEABLE pconstantToRepr #-}
  pconstantToRepr = fromIntegral
  {-# INLINEABLE pconstantFromRepr #-}
  pconstantFromRepr = toIntegralSized

-- | @since WIP
instance PEq PByte where
  {-# INLINEABLE (#==) #-}
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

-- | @since WIP
instance PPartialOrd PByte where
  {-# INLINEABLE (#<=) #-}
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  {-# INLINEABLE (#<) #-}
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

-- | @since WIP
instance POrd PByte

{- | Type designating whether logical operations should use padding or
truncation semantics. See
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#padding-versus-truncation-semantics)
for more details on this.

@since WIP
-}
newtype PLogicOpSemantics (s :: S) = PLogicOpSemantics (Term s PBool)
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
instance DerivePlutusType PLogicOpSemantics where
  type DPTStrat _ = PlutusTypeNewtype

{- | Indicates that padding semantics should be used.

@since WIP
-}
ppadding :: forall (s :: S). Term s PLogicOpSemantics
ppadding = pcon . PLogicOpSemantics . pcon $ PTrue

{- | Indicates that truncation semantics should be used.

@since WIP
-}
ptruncation :: forall (s :: S). Term s PLogicOpSemantics
ptruncation = pcon . PLogicOpSemantics . pcon $ PFalse

{- | Perform the logical AND of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicaland).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
pandBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
pandBS = phoistAcyclic $ plam $ \sem bs1 bs2 -> pmatch sem $ \(PLogicOpSemantics b) ->
  punsafeBuiltin PLC.AndByteString # b # bs1 # bs2

{- | Perform the logical OR of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalor).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
porBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
porBS = phoistAcyclic $ plam $ \sem bs1 bs2 -> pmatch sem $ \(PLogicOpSemantics b) ->
  punsafeBuiltin PLC.OrByteString # b # bs1 # bs2

{- | Perform the logical XOR of two 'PByteString's, as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalxor).
The 'PLogicOpSemantics' argument specifies what should be done if the lengths
of the two 'PByteString' arguments do not match.

@since WIP
-}
pxorBS ::
  forall (s :: S).
  Term s (PLogicOpSemantics :--> PByteString :--> PByteString :--> PByteString)
pxorBS = phoistAcyclic $ plam $ \sem bs1 bs2 -> pmatch sem $ \(PLogicOpSemantics b) ->
  punsafeBuiltin PLC.XorByteString # b # bs1 # bs2

{- | Perform the logical complement of a 'PByteString', as per
[CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122#bitwiselogicalcomplement).

@since WIP
-}
pcomplementBS ::
  forall (s :: S).
  Term s (PByteString :--> PByteString)
pcomplementBS = phoistAcyclic $ plam $ \bs -> punsafeBuiltin PLC.ComplementByteString # bs

{- | Construct a 'PByteString' of the specified length (0 if negative)
consisting entirely of zero bytes.

@since WIP
-}
pzeroesBS :: forall (s :: S). Term s (PInteger :--> PByteString)
pzeroesBS = phoistAcyclic $ plam go
  where
    go :: forall (s' :: S). Term s' PInteger -> Term s' PByteString
    go len = punsafeBuiltin PLC.ReplicateByte # pif (len #< 0) 0 len # (0 :: Term s' PInteger)

{- | Construct a 'PByteString' of the specified length (0 if negative)
consisting entirely of ones; that is, where every byte is @0xFF@.

@since WIP
-}
ponesBS :: forall (s :: S). Term s (PInteger :--> PByteString)
ponesBS = phoistAcyclic $ plam go
  where
    go :: forall (s' :: S). Term s' PInteger -> Term s' PByteString
    go len = punsafeBuiltin PLC.ReplicateByte # pif (len #< 0) 0 len # (255 :: Term s' PInteger)

{- | Given a desired length and a 'PByte', construct a 'PByteString' of the
specified length (0 if negative) consisting entirely of that 'PByte'.

@since WIP
-}
preplicateBS :: forall (s :: S). Term s (PInteger :--> PByte :--> PByteString)
preplicateBS = phoistAcyclic $ plam go
  where
    go :: forall (s' :: S). Term s' PInteger -> Term s' PByte -> Term s' PByteString
    go len w8 =
      punsafeBuiltin PLC.ReplicateByte
        # pif (len #< 0) 0 len
        # punsafeCoerce @_ @_ @PInteger w8

-- | Interpret a hex string as a PByteString.
phexByteStr :: HasCallStack => String -> Term s PByteString
phexByteStr = pconstant . BS.pack . f
  where
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

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
pintegerToByte = phoistAcyclic $ plam $ \i ->
  pif
    (i #< 0 #|| 255 #< i)
    perror -- We need to do this due to a dependency in Plutarch.String on ourselves
    (punsafeCoerce i)

{- | As 'pintegerToByte', but unchecked. Be /very/ careful with this: it can
potentially break downstream invariants and wreak all manner of
hard-to-discover havoc. Only included for reasons of efficiency.

@since WIP
-}
punsafeIntegerToByte :: Term s (PInteger :--> PByte)
punsafeIntegerToByte = phoistAcyclic $ plam punsafeCoerce

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

{- | Verify that the given predicate holds for every byte in the argument.

@since WIP
-}
pallBS ::
  forall (s :: S).
  Term s ((PByte :--> PBool) :--> PByteString :--> PBool)
pallBS = phoistAcyclic $ plam $ \p bs ->
  plet (plengthBS # bs) $ \len ->
    go p len bs # 0
  where
    go ::
      forall (s' :: S).
      Term s' (PByte :--> PBool) ->
      Term s' PInteger ->
      Term s' PByteString ->
      Term s' (PInteger :--> PBool)
    go p len bs = pfix #$ plam $ \self ix ->
      pif
        (ix #< len)
        ( pif
            (p #$ pindexBS # bs # ix)
            (self # (ix + 1))
            (pcon PFalse)
        )
        (pcon PTrue)

-- Helpers

hexDigitToWord8 :: HasCallStack => Char -> Word8
hexDigitToWord8 = f . toLower
  where
    f :: Char -> Word8
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f 'a' = 10
    f 'b' = 11
    f 'c' = 12
    f 'd' = 13
    f 'e' = 14
    f 'f' = 15
    f c = error ("InvalidHexDigit " <> [c])
