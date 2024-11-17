module Plutarch.Builtin.BLS (
  -- * Types
  PBLS12_381_G1_Element (..),
  PBLS12_381_G2_Element (..),
  PBLS12_381_MlResult (..),

  -- * Builtins
  pbuiltinBls12_381_G1_add,
  pbuiltinBls12_381_G1_scalarMul,
  pbuiltinBls12_381_G1_neg,
  pbuiltinBls12_381_G1_compress,
  pbuiltinBls12_381_G1_uncompress,
  pbuiltinBls12_381_G1_hashToGroup,
  pbuiltinBls12_381_G1_equal,
  pbuiltinBls12_381_G2_add,
  pbuiltinBls12_381_G2_scalarMul,
  pbuiltinBls12_381_G2_neg,
  pbuiltinBls12_381_G2_compress,
  pbuiltinBls12_381_G2_uncompress,
  pbuiltinBls12_381_G2_hashToGroup,
  pbuiltinBls12_381_G2_equal,
  pbuiltinBls12_381_millerLoop,
  pbuiltinBls12_381_mulMlResult,
  pbuiltinBls12_381_finalVerify,
) where

import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (
  S,
  Term,
  punsafeBuiltin,
  (:-->),
 )
import PlutusCore qualified as PLC

{- | A point on the BLS12-381 G1 curve.

@since WIP
-}
newtype PBLS12_381_G1_Element (s :: S)
  = PBLS12_381_G1_Element (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )

{- | A point on the BLS12-381 G2 curve.

@since WIP
-}
newtype PBLS12_381_G2_Element (s :: S)
  = PBLS12_381_G2_Element (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )

{- | The result of a Miller loop in BLS12-381 pairing.

@since WIP
-}
newtype PBLS12_381_MlResult (s :: S)
  = PBLS12_381_MlResult (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )

-- | @since WIP
pbuiltinBls12_381_G1_add ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_add = punsafeBuiltin PLC.Bls12_381_G1_add

-- | @since WIP
pbuiltinBls12_381_G1_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_scalarMul = punsafeBuiltin PLC.Bls12_381_G1_scalarMul

-- | @since WIP
pbuiltinBls12_381_G1_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_neg = punsafeBuiltin PLC.Bls12_381_G1_neg

-- | @since WIP
pbuiltinBls12_381_G1_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PByteString)
pbuiltinBls12_381_G1_compress = punsafeBuiltin PLC.Bls12_381_G1_compress

-- | @since WIP
pbuiltinBls12_381_G1_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_uncompress = punsafeBuiltin PLC.Bls12_381_G1_uncompress

-- | @since WIP
pbuiltinBls12_381_G1_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_hashToGroup = punsafeBuiltin PLC.Bls12_381_G1_hashToGroup

-- | @since WIP
pbuiltinBls12_381_G1_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBool)
pbuiltinBls12_381_G1_equal = punsafeBuiltin PLC.Bls12_381_G1_equal

-- | @since WIP
pbuiltinBls12_381_G2_add ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_add = punsafeBuiltin PLC.Bls12_381_G2_add

-- | @since WIP
pbuiltinBls12_381_G2_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_scalarMul = punsafeBuiltin PLC.Bls12_381_G2_scalarMul

-- | @since WIP
pbuiltinBls12_381_G2_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_neg = punsafeBuiltin PLC.Bls12_381_G2_neg

-- | @since WIP
pbuiltinBls12_381_G2_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PByteString)
pbuiltinBls12_381_G2_compress = punsafeBuiltin PLC.Bls12_381_G2_compress

-- | @since WIP
pbuiltinBls12_381_G2_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_uncompress = punsafeBuiltin PLC.Bls12_381_G2_uncompress

-- | @since WIP
pbuiltinBls12_381_G2_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_hashToGroup = punsafeBuiltin PLC.Bls12_381_G2_hashToGroup

-- | @since WIP
pbuiltinBls12_381_G2_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBool)
pbuiltinBls12_381_G2_equal = punsafeBuiltin PLC.Bls12_381_G2_equal

-- | @since WIP
pbuiltinBls12_381_millerLoop ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_MlResult)
pbuiltinBls12_381_millerLoop = punsafeBuiltin PLC.Bls12_381_millerLoop

-- | @since WIP
pbuiltinBls12_381_mulMlResult ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBLS12_381_MlResult)
pbuiltinBls12_381_mulMlResult = punsafeBuiltin PLC.Bls12_381_mulMlResult

-- | @since WIP
pbuiltinBls12_381_finalVerify ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBool)
pbuiltinBls12_381_finalVerify = punsafeBuiltin PLC.Bls12_381_finalVerify
