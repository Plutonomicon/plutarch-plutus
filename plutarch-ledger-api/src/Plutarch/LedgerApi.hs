{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | = Note

The 'Value.PValue', 'AssocMap.PMap' and 'Interval.PInterval'-related
functionality can be found in other modules, as these clash with the
Plutarch prelude. These should be imported qualified.
-}
module Plutarch.LedgerApi (
  -- * Contexts
  Contexts.PScriptContext (..),
  Contexts.PTxInfo (..),
  Contexts.PScriptInfo (..),
  Contexts.PScriptPurpose (..),

  -- * Tx

  -- ** Types
  Tx.PTxOutRef (..),
  Tx.PTxOut (..),
  Tx.PTxId (..),
  Contexts.PTxInInfo (..),
  Tx.POutputDatum (..),

  -- ** Functions
  pgetContinuingOutputs,
  pfindOwnInput,

  -- * Script

  -- ** Types
  Scripts.PDatum (..),
  Scripts.PDatumHash (..),
  Scripts.PRedeemer (..),
  Scripts.PRedeemerHash (..),
  Scripts.PScriptHash (..),

  -- ** Functions
  scriptHash,
  datumHash,
  redeemerHash,
  dataHash,
  pparseDatum,

  -- * Value
  Value.PValue (..),
  Value.AmountGuarantees (..),
  Value.PCurrencySymbol (..),
  Value.PTokenName (..),
  Value.PLovelace (..),

  -- * Assoc map

  -- ** Types
  AssocMap.PMap (..),
  AssocMap.KeyGuarantees (..),
  AssocMap.Commutativity (..),

  -- * Address
  Credential.PCredential (..),
  Credential.PStakingCredential (..),
  Address.PAddress (..),

  -- * Time
  Time.PPosixTime (..),

  -- * Interval
  Interval.PInterval (..),
  Interval.PLowerBound (..),
  Interval.PUpperBound (..),
  Interval.PExtended (..),

  -- * CIP-1694
  Contexts.PTxCert (..),
  Contexts.PDelegatee (..),
  Contexts.PDRepCredential (..),
  Contexts.PColdCommitteeCredential (..),
  Contexts.PHotCommitteeCredential (..),
  Contexts.PDRep (..),
  Contexts.PVoter (..),
  Contexts.PGovernanceActionId (..),
  Contexts.PVote (..),
  Contexts.PProtocolVersion (..),
  Contexts.PProposalProcedure (..),
  Contexts.PGovernanceAction (..),
  Contexts.PChangedParameters (..),
  Contexts.PConstitution (..),
  Contexts.PCommittee (..),

  -- * Crypto

  -- ** Types
  PubKey (..),
  Crypto.PPubKeyHash (..),
  pubKeyHash,

  -- * Utilities

  -- ** Types
  Utils.PMaybeData (..),
  Utils.PRationalData (..),

  -- ** Utilities
  pfromDJust,
  pisDJust,
  pmaybeData,
  pdjust,
  pdnothing,
  pmaybeToMaybeData,
  passertPDJust,
  Utils.prationalFromData,
) where

import Codec.Serialise (serialise)
import Crypto.Hash (
  Blake2b_224 (Blake2b_224),
  Blake2b_256 (Blake2b_256),
  hashWith,
 )
import Data.ByteArray (convert)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Short (fromShort)
import Data.Coerce (coerce)
import Plutarch.LedgerApi.Address qualified as Address
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Contexts qualified as Contexts
import Plutarch.LedgerApi.Credential qualified as Credential
import Plutarch.LedgerApi.Crypto qualified as Crypto
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Scripts qualified as Scripts
import Plutarch.LedgerApi.Time qualified as Time
import Plutarch.LedgerApi.Tx qualified as Tx
import Plutarch.LedgerApi.Utils qualified as Utils
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import Plutarch.Script (Script (unScript))
import PlutusLedgerApi.Common (serialiseUPLC)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

{- | Hash a script, appending the Plutus V2 prefix.

@since 2.0.0
-}
scriptHash :: Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x02"

-- | @since 2.0.0
newtype PubKey = PubKey
  { getPubKey :: Plutus.LedgerBytes
  -- ^ @since 2.0.0
  }
  deriving stock
    ( -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      Ord
    )
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
pubKeyHash :: PubKey -> Plutus.PubKeyHash
pubKeyHash = coerce hashLedgerBytes

-- | @since 2.0.0
datumHash :: Plutus.Datum -> Plutus.DatumHash
datumHash = coerce . dataHash

-- | @since 2.0.0
dataHash ::
  forall (a :: Type).
  Plutus.ToData a =>
  a ->
  PlutusTx.BuiltinByteString
dataHash = hashData . Plutus.toData

-- | @since 2.0.0
redeemerHash :: Plutus.Redeemer -> Plutus.RedeemerHash
redeemerHash = coerce . dataHash

{- | Find the output txns corresponding to the input being validated.

  Takes as arguments the inputs, outputs and the spending transaction referenced
  from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
          outputs = pfield @"outputs" # (getField @"txInfo" ctx)
      pure $ pgetContinuingOutputs # inputs # outputs # outRef
    _ ->
      pure $ ptraceInfoError "not a spending tx"
  @

  @since 2.1.0
-}
pgetContinuingOutputs ::
  forall (s :: S).
  Term s (PBuiltinList Contexts.PTxInInfo :--> PBuiltinList Tx.PTxOut :--> Tx.PTxOutRef :--> PBuiltinList Tx.PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) # outputs
      PNothing ->
        ptraceInfoError "can't get any continuing outputs"
  where
    matches ::
      forall (s' :: S).
      Term s' (Address.PAddress :--> Tx.PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

{- | Find the input being spent in the current transaction.

  Takes as arguments the inputs, as well as the spending transaction referenced from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceInfoError "not a spending tx"
  @

  @since 2.1.0
-}
pfindOwnInput ::
  forall (s :: S).
  Term s (PBuiltinList Contexts.PTxInInfo :--> Tx.PTxOutRef :--> PMaybe Contexts.PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches ::
      forall (s' :: S).
      Term s' (Tx.PTxOutRef :--> Contexts.PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

{- | Lookup up the datum given the datum hash.

  Takes as argument the datum assoc list from a `PTxInfo`. Validates the datum
  using `PTryFrom`.

  __Example:__

  @
  pparseDatum @MyType # datumHash #$ pfield @"datums" # txinfo
  @

  @since 2.1.2
-}
pparseDatum ::
  forall (a :: S -> Type) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s (Scripts.PDatumHash :--> AssocMap.PMap 'AssocMap.Unsorted Scripts.PDatumHash Scripts.PDatum :--> PMaybe (PAsData a))
pparseDatum = phoistAcyclic $ plam $ \dh datums ->
  pmatch (AssocMap.plookup # dh # datums) $ \case
    PNothing -> pcon PNothing
    PJust datum -> pcon . PJust $ ptryFrom (pto datum) fst

{- | Extracts the element out of a 'PDJust' and throws an error if its
argument is 'PDNothing'.

@since 2.1.1
-}
pfromDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (Utils.PMaybeData a :--> a)
pfromDJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    Utils.PDNothing _ -> ptraceInfoError "pfromDJust: found PDNothing"
    Utils.PDJust x -> pfromData $ pfield @"_0" # x

{- | Yield 'PTrue' if a given 'PMaybeData' is of the form @'PDJust' _@.

@since 2.1.1
-}
pisDJust ::
  forall (a :: PType) (s :: S).
  Term s (Utils.PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    Utils.PDJust _ -> pconstant True
    _ -> pconstant False

{- | Special version of 'pmaybe' that works with 'PMaybeData'.

@since 2.1.1
-}
pmaybeData ::
  forall (a :: PType) (b :: PType) (s :: S).
  PIsData a =>
  Term s (b :--> (a :--> b) :--> Utils.PMaybeData a :--> b)
pmaybeData = phoistAcyclic $
  plam $ \d f m -> pmatch m $
    \case
      Utils.PDJust x -> f #$ pfield @"_0" # x
      _ -> d

{- | Construct a 'PDJust' value.

@since 2.1.1
-}
pdjust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (a :--> Utils.PMaybeData a)
pdjust = phoistAcyclic $
  plam $
    \x -> pcon $ Utils.PDJust $ pdcons @"_0" # pdata x #$ pdnil

{- | Construct a 'PDNothing' value.

@since 2.1.1
-}
pdnothing ::
  forall (a :: PType) (s :: S).
  Term s (Utils.PMaybeData a)
pdnothing = phoistAcyclic $ pcon $ Utils.PDNothing pdnil

{- | Construct a 'PMaybeData' given a 'PMaybe'. Could be useful if you want to
"lift" from 'PMaybe' to 'Maybe'.

@since 2.1.1
-}
pmaybeToMaybeData ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybe a :--> Utils.PMaybeData a)
pmaybeToMaybeData = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> pcon $ Utils.PDNothing pdnil
    PJust x -> pcon $ Utils.PDJust $ pdcons @"_0" # pdata x # pdnil

{- | Extract the value stored in a 'PMaybeData' container. If there's no value,
throw an error with the given message.

@since 2.1.1
-}
passertPDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PString :--> Utils.PMaybeData a :--> a)
passertPDJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    Utils.PDJust ((pfield @"_0" #) -> v) -> v
    _ -> ptraceInfoError emsg

-- Helpers

hashScriptWithPrefix :: ByteString -> Script -> Plutus.ScriptHash
hashScriptWithPrefix prefix scr =
  Plutus.ScriptHash . hashBlake2b_224 $
    prefix <> (fromShort . serialiseUPLC . unScript $ scr)

hashLedgerBytes :: Plutus.LedgerBytes -> PlutusTx.BuiltinByteString
hashLedgerBytes = hashBlake2b_224 . PlutusTx.fromBuiltin . Plutus.getLedgerBytes

hashBlake2b_224 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_224 = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith Blake2b_224

hashBlake2b_256 :: ByteString -> PlutusTx.BuiltinByteString
hashBlake2b_256 = PlutusTx.toBuiltin . convert @_ @ByteString . hashWith Blake2b_256

hashData :: Plutus.Data -> PlutusTx.BuiltinByteString
hashData = hashBlake2b_256 . toStrict . serialise