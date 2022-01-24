{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1 (
  -- * V1 Specific types
  PScriptContext (..),
  PTxInfo (..),

  -- * General types, compatible with V1 and V2
  PScriptPurpose (..),

  -- ** Script
  PDatum (..),
  PRedeemer (..),
  PDatumHash (..),
  PRedeemerHash (..),
  PValidatorHash (..),
  PStakeValidatorHash (..),

  -- ** Value
  PValue (..),
  PCurrencySymbol (..),
  PTokenName (..),

  -- ** Crypto
  PPubKeyHash (..),
  PPubKey (..),
  PSignature (..),

  -- ** Time
  PPOSIXTime (..),
  type PPOSIXTimeRange,

  -- ** Interval
  PInterval (..),
  PLowerBound (..),
  PUpperBound (..),
  PExtended (..),
  type PClosure,

  -- ** Address
  PAddress (..),
  PCredential (..),
  PStakingCredential (..),

  -- ** Tx
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef (..),
  PTxId (..),
  PDCert (..),

  -- ** AssocMap
  PMap (..),

  -- ** Others
  PMaybe (..),
  PEither (..),
) where

--------------------------------------------------------------------------------

import Plutarch (PMatch, PlutusType)
import Plutarch.Bool (PBool)
import Plutarch.Builtin (PAsData, PBuiltinList, PData, PIsData, type PBuiltinMap)
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PDataRecord,
  PDataSum,
  PIsDataRepr,
  PIsDataReprInstances (PIsDataReprInstances),
  PLabeledType ((:=)),
 )
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstant,
  PConstantRepr,
  PConstanted,
  PLift,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )

-- ctor in-scope for deriving
import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as PlutusCrpyto
import qualified PlutusTx.AssocMap as PlutusMap
import qualified PlutusTx.Builtins.Internal as PT

--------------------------------------------------------------------------------
type PTuple a b =
  PDataSum
    '[ '[ "_0" ':= a
        , "_1" ':= b
        ]
     ]

---------- V1 Specific types, Incompatible with V2

newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut)
               , "fee" ':= PValue
               , "mint" ':= PValue
               , "dcert" ':= PBuiltinList (PAsData PDCert)
               , "wdrl" ':= PBuiltinList (PAsData (PTuple PStakingCredential PInteger))
               , "validRange" ':= PPOSIXTimeRange
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "data" ':= PBuiltinList (PAsData (PTuple PDatumHash PDatum))
               , "id" ':= PTxId
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PTxInfo

instance PUnsafeLiftDecl PTxInfo where type PLifted PTxInfo = Plutus.TxInfo
deriving via (DerivePConstantViaData Plutus.TxInfo PTxInfo) instance (PConstant Plutus.TxInfo)

newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PScriptContext

instance PUnsafeLiftDecl PScriptContext where type PLifted PScriptContext = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance (PConstant Plutus.ScriptContext)

-- General types, used by V1 and V2

data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PScriptPurpose)

instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
deriving via (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) instance (PConstant Plutus.ScriptPurpose)

---------- Scripts

newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving (PlutusType, PIsData) via (DerivePNewtype PDatum PData)

instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaNewtype Plutus.Datum PDatum PData) instance (PConstant Plutus.Datum)

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving (PlutusType, PIsData) via (DerivePNewtype PRedeemer PData)

instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaNewtype Plutus.Redeemer PRedeemer PData) instance (PConstant Plutus.Redeemer)

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PDatumHash PByteString)

instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaNewtype Plutus.DatumHash PDatumHash PByteString) instance (PConstant Plutus.DatumHash)

newtype PStakeValidatorHash (s :: S) = PStakeValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PStakeValidatorHash PByteString)

instance PUnsafeLiftDecl PStakeValidatorHash where type PLifted PStakeValidatorHash = Plutus.StakeValidatorHash
deriving via (DerivePConstantViaNewtype Plutus.StakeValidatorHash PStakeValidatorHash PByteString) instance (PConstant Plutus.StakeValidatorHash)

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PRedeemerHash PByteString)

instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via (DerivePConstantViaNewtype Plutus.RedeemerHash PRedeemerHash PByteString) instance (PConstant Plutus.RedeemerHash)

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PValidatorHash PByteString)

instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
deriving via (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString) instance (PConstant Plutus.ValidatorHash)

---------- Value

newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving newtype (Semigroup, Monoid)
  deriving (PlutusType, PIsData) via (DerivePNewtype PTokenName PByteString)

instance PUnsafeLiftDecl PTokenName where type PLifted PTokenName = Plutus.TokenName
deriving via
  (DerivePConstantViaNewtype Plutus.TokenName PTokenName PByteString)
  instance
    (PConstant Plutus.TokenName)

newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PCurrencySymbol PByteString)

instance PUnsafeLiftDecl PCurrencySymbol where type PLifted PCurrencySymbol = Plutus.CurrencySymbol
deriving via
  (DerivePConstantViaNewtype Plutus.CurrencySymbol PCurrencySymbol PByteString)
  instance
    (PConstant Plutus.CurrencySymbol)

newtype PValue (s :: S) = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    ( PlutusType
    , PIsData
    )
    via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))

instance PUnsafeLiftDecl PValue where type PLifted PValue = Plutus.Value
deriving via
  (DerivePConstantViaNewtype Plutus.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  instance
    (PConstant Plutus.Value)

---------- Crypto

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PPubKeyHash PByteString)

instance PUnsafeLiftDecl PPubKeyHash where type PLifted PPubKeyHash = Plutus.PubKeyHash
deriving via
  (DerivePConstantViaNewtype Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    (PConstant Plutus.PubKeyHash)

newtype PPubKey (s :: S) = PPubKey (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PPubKey PByteString)

instance PUnsafeLiftDecl PPubKey where type PLifted PPubKey = PlutusCrpyto.PubKey
deriving via
  (DerivePConstantViaNewtype PlutusCrpyto.PubKey PPubKey PByteString)
  instance
    (PConstant PlutusCrpyto.PubKey)

newtype PSignature (s :: S) = PSignature (Term s PByteString)
  deriving (PlutusType, PIsData) via (DerivePNewtype PSignature PByteString)

instance PUnsafeLiftDecl PSignature where type PLifted PSignature = PlutusCrpyto.Signature
deriving via
  (DerivePConstantViaNewtype PlutusCrpyto.Signature PSignature PByteString)
  instance
    (PConstant PlutusCrpyto.Signature)

---------- Time

newtype PPOSIXTime (s :: S)
  = PPOSIXTime (Term s PInteger)
  deriving (PIntegral) via (PInteger)
  deriving newtype (Num)
  deriving (PlutusType, PIsData) via (DerivePNewtype PPOSIXTime PInteger)

instance PUnsafeLiftDecl PPOSIXTime where type PLifted PPOSIXTime = Plutus.POSIXTime
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPOSIXTime PInteger)
  instance
    (PConstant Plutus.POSIXTime)

type PPOSIXTimeRange = PInterval PPOSIXTime

---------- Interval

type PClosure = PBool

newtype PInterval a (s :: S)
  = PInterval
      ( Term
          s
          ( PDataRecord
              '[ "from" ':= PLowerBound a
               , "to" ':= PUpperBound a
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances (PInterval a)

newtype PLowerBound a (s :: S)
  = PLowerBound
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PExtended a
               , "_1" ':= PClosure
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances (PLowerBound a))

newtype PUpperBound a (s :: S)
  = PUpperBound
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PExtended a
               , "_1" ':= PClosure
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances (PUpperBound a))

data PExtended a (s :: S)
  = PNegInf (Term s (PDataRecord '[]))
  | PFinite (Term s (PDataRecord '["_0" ':= a]))
  | PPosInf (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances (PExtended a))

---------- Tx/Address

data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PValidatorHash]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PCredential)

data PStakingCredential (s :: S)
  = PStakingHash (Term s (PDataRecord '["_0" ':= PCredential]))
  | PStakingPtr
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PInteger
               , "_1" ':= PInteger
               , "_2" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PStakingCredential

newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataRecord
              '[ "credential" ':= PCredential
               , "stakingCredential" ':= (PMaybe PStakingCredential)
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PAddress

---------- Tx

newtype PTxId (s :: S)
  = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PTxId

newtype PTxOutRef (s :: S)
  = PTxOutRef
      ( Term
          s
          ( PDataRecord
              '[ "id" ':= PTxId
               , "idx" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PTxOutRef

newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PTxInInfo

newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= PValue
               , "datumHash" ':= PMaybe PDatumHash
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances PTxOut)

data PDCert (s :: S)
  = PDCertDelegRegKey (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PDCertDelegDeRegKey (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PDCertDelegDelegate
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PStakingCredential
               , "_1" ':= PPubKeyHash
               ]
          )
      )
  | PDCertPoolRegister (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PPubKeyHash]))
  | PDCertPoolRetire (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PInteger]))
  | PDCertGenesis (Term s (PDataRecord '[]))
  | PDCertMir (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PDCert)

---------- AssocMap

newtype PMap (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving (PlutusType, PIsData) via (DerivePNewtype (PMap k v) (PBuiltinMap k v))

instance
  ( Plutus.ToData (PLifted v)
  , Plutus.ToData (PLifted k)
  , Plutus.FromData (PLifted v)
  , Plutus.FromData (PLifted k)
  , PLift k
  , PLift v
  ) =>
  PUnsafeLiftDecl (PMap k v)
  where
  type PLifted (PMap k v) = PlutusMap.Map (PLifted k) (PLifted v)

instance
  ( PLifted (PConstanted k) ~ k
  , Plutus.ToData v
  , Plutus.FromData v
  , Plutus.ToData k
  , Plutus.FromData k
  , PConstant k
  , PLifted (PConstanted v) ~ v
  , Plutus.FromData v
  , Plutus.ToData v
  , PConstant v
  ) =>
  PConstant (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap (PConstanted k) (PConstanted v)
  pconstantToRepr m = (\(x, y) -> (Plutus.toData x, Plutus.toData y)) <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    flip traverse m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

---------- Others

data PMaybe a (s :: S)
  = PNothing (Term s (PDataRecord '[]))
  | PJust (Term s (PDataRecord '["_0" ':= a]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances (PMaybe a)

data PEither a b (s :: S)
  = PLeft (Term s (PDataRecord '["_0" ':= a]))
  | PRight (Term s (PDataRecord '["_0" ':= b]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances (PEither a b)
