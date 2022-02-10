{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Scripts (
  -- * Plutus API Types
  PDatum (PDatum),
  PDatumHash (PDatumHash),
  PRedeemer (PRedeemer),
  PRedeemerHash (PRedeemerHash),
  PStakeValidatorHash (PStakeValidatorHash),
  PValidatorHash (PValidatorHash),

  -- * Script Term Types
  type PValidator,
  type PMintingPolicy,
  type PStakeValidator,

  -- * Compiled Script Types
  Validator (Validator, validatorScript, validatorTerm),
  MintingPolicy (MintingPolicy, mintingPolicyScript, mintingPolicyTerm),
  StakeValidator (StakeValidator, stakeValidatorScript, stakeValidatorTerm),

  -- * Constructors
  mkValidator,
  mkMintingPolicy,
  mkStakeValidator,

  -- * Script helpers
  pwrapValidatorWith,
  pwrapValidatorAsData,
  pwrapValidatorFromData,
  pwrapMintingPolicyWith,
  pwrapMintingPolicyAsData,
  pwrapMintingPolicyFromData,
  pwrapStakeValidatorWith,
  pwrapStakeValidatorAsData,
  pwrapStakeValidatorFromData,

  -- * Serialisation
  serialiseScript,
  serialiseValidator,
  serialiseMintingPolicy,
  serialiseStakeValidator,

  -- * Script Hashing
  scriptHash,
  validatorHash,
  mintingPolicySymbol,
  stakeValidatorHash,

  -- * Datum Hashing
  datumHash,
) where

import qualified Data.ByteString.Lazy as Lazy
import Data.Coerce (coerce)

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx.Builtins.Internal as PT

import Plutarch (ClosedTerm, POpaque, compile, popaque)
import Plutarch.Api.Internal.Scripts (
  datumHash,
  hashScriptWithPrefix,
  serialiseScript,
 )
import Plutarch.Builtin (punsafeAsData, punsafeFromData)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PDatum PData)

instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaNewtype Plutus.Datum PDatum PData) instance (PConstant Plutus.Datum)

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PRedeemer PData)

instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaNewtype Plutus.Redeemer PRedeemer PData) instance (PConstant Plutus.Redeemer)

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PDatumHash PByteString)

instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaNewtype Plutus.DatumHash PDatumHash PByteString) instance (PConstant Plutus.DatumHash)

newtype PStakeValidatorHash (s :: S) = PStakeValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PStakeValidatorHash PByteString)

instance PUnsafeLiftDecl PStakeValidatorHash where type PLifted PStakeValidatorHash = Plutus.StakeValidatorHash
deriving via
  (DerivePConstantViaNewtype Plutus.StakeValidatorHash PStakeValidatorHash PByteString)
  instance
    (PConstant Plutus.StakeValidatorHash)

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PRedeemerHash PByteString)

instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via
  (DerivePConstantViaNewtype Plutus.RedeemerHash PRedeemerHash PByteString)
  instance
    (PConstant Plutus.RedeemerHash)

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PValidatorHash PByteString)

instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
deriving via
  (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString)
  instance
    (PConstant Plutus.ValidatorHash)

-----------------------------
-- On-chain Script Types

-- | a Validator Term
type PValidator = PData :--> PData :--> PData :--> POpaque

-- | a MintingPolicy Term
type PMintingPolicy = PData :--> PData :--> POpaque

-- | a StakeValidator Term
type PStakeValidator = PData :--> PData :--> POpaque

-- | A compiled Validator, bundled with the Plutarch Term
data Validator = Validator
  { validatorScript :: Plutus.Script
  , validatorTerm :: ClosedTerm PValidator
  }

-- | A compiled MintingPolicy, bundled with the Plutarch Term
data MintingPolicy = MintingPolicy
  { mintingPolicyScript :: Plutus.Script
  , mintingPolicyTerm :: ClosedTerm PMintingPolicy
  }

-- | A compiled StakeValidator, bundled with the Plutarch Term
data StakeValidator = StakeValidator
  { stakeValidatorScript :: Plutus.Script
  , stakeValidatorTerm :: ClosedTerm PStakeValidator
  }

-- | Compile a Validator
mkValidator :: ClosedTerm PValidator -> Validator
mkValidator term =
  Validator (compile term) term

-- | Compile a MintingPolicy
mkMintingPolicy :: ClosedTerm PMintingPolicy -> MintingPolicy
mkMintingPolicy term =
  MintingPolicy (compile term) term

-- | Compile a StakeValidator
mkStakeValidator :: ClosedTerm PStakeValidator -> StakeValidator
mkStakeValidator term =
  StakeValidator (compile term) term

-- | Serialise a Validator to a (Lazy) ByteString
serialiseValidator :: Validator -> Lazy.ByteString
serialiseValidator = serialiseScript . validatorScript

-- | Serialise a StakeValidator to a (Lazy) ByteString
serialiseStakeValidator :: StakeValidator -> Lazy.ByteString
serialiseStakeValidator = serialiseScript . stakeValidatorScript

-- | Serialise a MintingPolicy to a (Lazy) ByteString
serialiseMintingPolicy :: MintingPolicy -> Lazy.ByteString
serialiseMintingPolicy = serialiseScript . mintingPolicyScript

-- | Hash a Script, with the correct prefix for Plutus V1
scriptHash :: Plutus.Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x01"

-- | Hash a Validator, with the correct prefix for Plutus V1
validatorHash :: Validator -> Plutus.ValidatorHash
validatorHash = coerce . scriptHash . validatorScript

-- | Hash a MintingPolicy, with the correct prefix for Plutus V1
mintingPolicySymbol :: MintingPolicy -> Plutus.CurrencySymbol
mintingPolicySymbol = coerce . scriptHash . mintingPolicyScript

-- | Hash a StakeValidator, with the correct prefix for Plutus V1
stakeValidatorHash :: StakeValidator -> Plutus.StakeValidatorHash
stakeValidatorHash = coerce . scriptHash . stakeValidatorScript

{- |
  Construct a `PValidator`, assuming the `PData` arguments are
  of the correct form.

  Shorthand for:
  @pwrapValidatorWith punsafeAsData punsafeAsData punsafeAsData@
-}
pwrapValidatorAsData ::
  ( forall s.
    Term s (PAsData datum) ->
    Term s (PAsData redeemer) ->
    Term s (PAsData ctx) ->
    Term s p
  ) ->
  ClosedTerm PValidator
pwrapValidatorAsData = pwrapValidatorWith punsafeAsData punsafeAsData punsafeAsData

{- |
  Construct a `PValidator`, assuming the `PData` arguments are
  of the correct form, wrapping them with `pfromData`.

  Shorthand for:
  @pwrapValidatorWith punsafeFromData punsafeFromData punsafeFromData@
-}
pwrapValidatorFromData ::
  ( PIsData datum
  , PIsData redeemer
  , PIsData context
  ) =>
  ( forall s.
    Term s datum ->
    Term s redeemer ->
    Term s context ->
    Term s p
  ) ->
  ClosedTerm PValidator
pwrapValidatorFromData =
  pwrapValidatorWith punsafeFromData punsafeFromData punsafeFromData

{- |
  Construct a `PValidator` using the provided wrappers
  for each of the arguments.
-}
pwrapValidatorWith ::
  forall datum redeemer ctx p.
  (forall s. Term s PData -> Term s datum) ->
  (forall s. Term s PData -> Term s redeemer) ->
  (forall s. Term s PData -> Term s ctx) ->
  (forall s. Term s datum -> Term s redeemer -> Term s ctx -> Term s p) ->
  ClosedTerm PValidator
pwrapValidatorWith wrapD wrapR wrapCtx validator =
  plam $ \datum redeemer ctx ->
    popaque $ validator (wrapD datum) (wrapR redeemer) (wrapCtx ctx)

{- |
  Construct a `PMintingPolicy`, assuming the `PData` arguments are
  of the correct form.

  Shorthand for:
  @pwrapMintingPolicyWith punsafeAsData punsafeAsData@
-}
pwrapMintingPolicyAsData ::
  ( forall s.
    Term s (PAsData redeemer) ->
    Term s (PAsData ctx) ->
    Term s p
  ) ->
  ClosedTerm PMintingPolicy
pwrapMintingPolicyAsData = pwrapMintingPolicyWith punsafeAsData punsafeAsData

{- |
  Construct a `PMintingPolicy`, assuming the `PData` arguments are
  of the correct form, wrapping them with `pfromData`.

  Shorthand for:
  @pwrapMintingPolicyWith punsafeFromData punsafeFromData@
-}
pwrapMintingPolicyFromData ::
  ( PIsData redeemer
  , PIsData context
  ) =>
  ( forall s.
    Term s redeemer ->
    Term s context ->
    Term s p
  ) ->
  ClosedTerm PMintingPolicy
pwrapMintingPolicyFromData =
  pwrapMintingPolicyWith punsafeFromData punsafeFromData

{- |
  Construct a `PMintingPolicy` using the provided wrappers
  for each of the arguments.
-}
pwrapMintingPolicyWith ::
  forall redeemer ctx p.
  (forall s. Term s PData -> Term s redeemer) ->
  (forall s. Term s PData -> Term s ctx) ->
  (forall s. Term s redeemer -> Term s ctx -> Term s p) ->
  ClosedTerm PMintingPolicy
pwrapMintingPolicyWith wrapR wrapCtx policy =
  plam $ \redeemer ctx ->
    popaque $ policy (wrapR redeemer) (wrapCtx ctx)

{- |
  Construct a `PStakeValidator`, assuming the `PData` arguments are
  of the correct form.

  Shorthand for:
  @pwrapStakeValidatorWith punsafeAsData punsafeAsData@
-}
pwrapStakeValidatorAsData ::
  ( forall s.
    Term s (PAsData redeemer) ->
    Term s (PAsData ctx) ->
    Term s p
  ) ->
  ClosedTerm PStakeValidator
pwrapStakeValidatorAsData = pwrapStakeValidatorWith punsafeAsData punsafeAsData

{- |
  Construct a `PStakeValidator`, assuming the `PData` arguments are
  of the correct form, wrapping them with `pfromData`.

  Shorthand for:
  @pwrapStakeValidatorWith punsafeFromData punsafeFromData@
-}
pwrapStakeValidatorFromData ::
  ( PIsData redeemer
  , PIsData context
  ) =>
  ( forall s.
    Term s redeemer ->
    Term s context ->
    Term s p
  ) ->
  ClosedTerm PStakeValidator
pwrapStakeValidatorFromData =
  pwrapStakeValidatorWith punsafeFromData punsafeFromData

{- |
  Construct a `PStakeValidator` using the provided wrappers
  for each of the arguments.
-}
pwrapStakeValidatorWith ::
  forall redeemer ctx p.
  (forall s. Term s PData -> Term s redeemer) ->
  (forall s. Term s PData -> Term s ctx) ->
  (forall s. Term s redeemer -> Term s ctx -> Term s p) ->
  ClosedTerm PStakeValidator
pwrapStakeValidatorWith wrapR wrapCtx policy =
  plam $ \redeemer ctx ->
    popaque $ policy (wrapR redeemer) (wrapCtx ctx)
