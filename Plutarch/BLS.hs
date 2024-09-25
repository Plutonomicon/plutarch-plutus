{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.BLS (
  PBuiltinBLS12_381_G1_Element (..),
  pbls12_381_G1_add,
  pbls12_381_G1_scalarMul,
  pbls12_381_G1_neg,
  pbls12_381_G1_compress,
  pbls12_381_G1_uncompress,
  pbls12_381_G1_hashToGroup,
  pbls12_381_G1_compressed_zero,
  pbls12_381_G1_compressed_generator,
  PBuiltinBLS12_381_G2_Element (..),
  pbls12_381_G2_add,
  pbls12_381_G2_scalarMul,
  pbls12_381_G2_neg,
  pbls12_381_G2_compress,
  pbls12_381_G2_uncompress,
  pbls12_381_G2_hashToGroup,
  pbls12_381_G2_compressed_zero,
  pbls12_381_G2_compressed_generator,
  PBuiltinBLS12_381_MlResult (..),
  pbls12_381_millerLoop,
  pbls12_381_mulMlResult,
  pbls12_381_finalVerify,
) where

import GHC.Generics (Generic)
import Plutarch.Bool (PBool, PEq, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (Term, (#), (:-->))
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS12_381.G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS12_381.G2
import PlutusCore.Crypto.BLS12_381.Pairing qualified as BLS12_381.Pairing

newtype PBuiltinBLS12_381_G1_Element s = PBuiltinBLS12_381_G1_Element (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PBuiltinBLS12_381_G1_Element where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PBuiltinBLS12_381_G1_Element where type PLifted PBuiltinBLS12_381_G1_Element = BLS12_381.G1.Element
deriving via (DerivePConstantDirect BLS12_381.G1.Element PBuiltinBLS12_381_G1_Element) instance PConstantDecl BLS12_381.G1.Element

instance PEq PBuiltinBLS12_381_G1_Element where
  x #== y = punsafeBuiltin PLC.Bls12_381_G1_equal # x # y

pbls12_381_G1_add :: Term s (PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_add = punsafeBuiltin PLC.Bls12_381_G1_add

pbls12_381_G1_scalarMul :: Term s (PInteger :--> PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_scalarMul = punsafeBuiltin PLC.Bls12_381_G1_scalarMul

pbls12_381_G1_neg :: Term s (PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_neg = punsafeBuiltin PLC.Bls12_381_G1_neg

pbls12_381_G1_compress :: Term s (PBuiltinBLS12_381_G1_Element :--> PByteString)
pbls12_381_G1_compress = punsafeBuiltin PLC.Bls12_381_G1_compress

pbls12_381_G1_uncompress :: Term s (PByteString :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_uncompress = punsafeBuiltin PLC.Bls12_381_G1_uncompress

pbls12_381_G1_hashToGroup :: Term s (PByteString :--> PByteString :--> PBuiltinBLS12_381_G1_Element)
pbls12_381_G1_hashToGroup = punsafeBuiltin PLC.Bls12_381_G1_hashToGroup

pbls12_381_G1_compressed_zero :: Term s PByteString
pbls12_381_G1_compressed_zero = pconstant BLS12_381.G1.compressed_zero

pbls12_381_G1_compressed_generator :: Term s PByteString
pbls12_381_G1_compressed_generator = pconstant BLS12_381.G1.compressed_generator

newtype PBuiltinBLS12_381_G2_Element s = PBuiltinBLS12_381_G2_Element (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PBuiltinBLS12_381_G2_Element where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PBuiltinBLS12_381_G2_Element where type PLifted PBuiltinBLS12_381_G2_Element = BLS12_381.G2.Element
deriving via (DerivePConstantDirect BLS12_381.G2.Element PBuiltinBLS12_381_G2_Element) instance PConstantDecl BLS12_381.G2.Element

instance PEq PBuiltinBLS12_381_G2_Element where
  x #== y = punsafeBuiltin PLC.Bls12_381_G2_equal # x # y

pbls12_381_G2_add :: Term s (PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_add = punsafeBuiltin PLC.Bls12_381_G2_add

pbls12_381_G2_scalarMul :: Term s (PInteger :--> PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_scalarMul = punsafeBuiltin PLC.Bls12_381_G2_scalarMul

pbls12_381_G2_neg :: Term s (PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_neg = punsafeBuiltin PLC.Bls12_381_G2_neg

pbls12_381_G2_compress :: Term s (PBuiltinBLS12_381_G2_Element :--> PByteString)
pbls12_381_G2_compress = punsafeBuiltin PLC.Bls12_381_G2_compress

pbls12_381_G2_uncompress :: Term s (PByteString :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_uncompress = punsafeBuiltin PLC.Bls12_381_G2_uncompress

pbls12_381_G2_hashToGroup :: Term s (PByteString :--> PByteString :--> PBuiltinBLS12_381_G2_Element)
pbls12_381_G2_hashToGroup = punsafeBuiltin PLC.Bls12_381_G2_hashToGroup

pbls12_381_G2_compressed_zero :: Term s PByteString
pbls12_381_G2_compressed_zero = pconstant BLS12_381.G2.compressed_zero

pbls12_381_G2_compressed_generator :: Term s PByteString
pbls12_381_G2_compressed_generator = pconstant BLS12_381.G2.compressed_generator

newtype PBuiltinBLS12_381_MlResult s = PBuiltinBLS12_381_MlResult (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PBuiltinBLS12_381_MlResult where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PBuiltinBLS12_381_MlResult where type PLifted PBuiltinBLS12_381_MlResult = BLS12_381.Pairing.MlResult
deriving via (DerivePConstantDirect BLS12_381.Pairing.MlResult PBuiltinBLS12_381_MlResult) instance PConstantDecl BLS12_381.Pairing.MlResult

pbls12_381_millerLoop :: Term s (PBuiltinBLS12_381_G1_Element :--> PBuiltinBLS12_381_G2_Element :--> PBuiltinBLS12_381_MlResult)
pbls12_381_millerLoop = punsafeBuiltin PLC.Bls12_381_millerLoop

pbls12_381_mulMlResult :: Term s (PBuiltinBLS12_381_MlResult :--> PBuiltinBLS12_381_MlResult :--> PBuiltinBLS12_381_MlResult)
pbls12_381_mulMlResult = punsafeBuiltin PLC.Bls12_381_mulMlResult

pbls12_381_finalVerify :: Term s (PBuiltinBLS12_381_MlResult :--> PBuiltinBLS12_381_MlResult :--> PBool)
pbls12_381_finalVerify = punsafeBuiltin PLC.Bls12_381_finalVerify
