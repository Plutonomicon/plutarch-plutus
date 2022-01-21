{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}

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
  DataReprHandlers (DRHCons, DRHNil),
  DerivePConstantViaData (DerivePConstantViaData),
  PDataRecord,
  PIsDataRepr,
  PIsDataReprInstances (PIsDataReprInstances),
  PIsDataReprRepr,
  PLabeled (..),
  pmatchDataRepr,
  pmatchRepr,
 )
import Plutarch.Field (DerivePDataFields (..), PDataFields (..))
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype), PConstant, PLifted, PUnsafeLiftDecl)

-- ctor in-scope for deriving
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as PlutusCrpyto
import qualified PlutusTx.Builtins.Internal as PT

--------------------------------------------------------------------------------
type PTuple a b =
  PDataRecord
    '[ "_0" ':= a
     , "_1" ':= b
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
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PTxInfo
  deriving (PDataFields) via (DerivePDataFields PTxInfo)

instance PUnsafeLiftDecl PTxInfo where type PLifted PTxInfo = Plutus.TxInfo
deriving via (DerivePConstantViaData Plutus.TxInfo PTxInfo) instance (PConstant Plutus.TxInfo)

instance PIsDataRepr PTxInfo where
  type
    PIsDataReprRepr PTxInfo =
      '[ '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
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
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PTxInfo)) $ DRHNil)

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
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PScriptContext
  deriving (PDataFields) via (DerivePDataFields PScriptContext)

instance PUnsafeLiftDecl PScriptContext where type PLifted PScriptContext = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance (PConstant Plutus.ScriptContext)

instance PIsDataRepr PScriptContext where
  type
    PIsDataReprRepr PScriptContext =
      '[ '[ "txInfo" ':= PTxInfo
          , "purpose" ':= PScriptPurpose
          ]
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PScriptContext)) $ DRHNil)

-- General types, used by V1 and V2

data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PScriptPurpose)
  deriving (PDataFields) via (DerivePDataFields PScriptPurpose)

instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
deriving via (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) instance (PConstant Plutus.ScriptPurpose)

instance PIsDataRepr PScriptPurpose where
  type
    PIsDataReprRepr PScriptPurpose =
      '[ '["_0" ':= PCurrencySymbol]
       , '["_0" ':= PTxOutRef]
       , '["_0" ':= PStakingCredential]
       , '["_0" ':= PDCert]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PMinting) $
        DRHCons (f . PSpending) $
          DRHCons (f . PRewarding) $
            DRHCons
              (f . PCertifying)
              DRHNil

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

-- FIXME: This fails typecheck. Representations do not match.
-- instance PUnsafeLiftDecl PValue where type PLifted PValue = Plutus.Value
-- deriving via
--   (DerivePConstantViaNewtype Plutus.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
--   instance
--     (PConstant Plutus.Value)

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
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances
          (PInterval a)
  deriving (PDataFields) via (DerivePDataFields (PInterval a))

instance PIsDataRepr (PInterval a) where
  type
    PIsDataReprRepr (PInterval a) =
      '[ '[ "from" ':= PLowerBound a
          , "to" ':= PUpperBound a
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PInterval) DRHNil

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
  deriving
    ( PMatch
    , PIsData
    )
    via ( PIsDataReprInstances
            (PLowerBound a)
        )
  deriving (PDataFields) via (DerivePDataFields (PLowerBound a))

instance PIsDataRepr (PLowerBound a) where
  type
    PIsDataReprRepr (PLowerBound a) =
      '[ '[ "_0" ':= PExtended a
          , "_1" ':= PClosure
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLowerBound) DRHNil

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
  deriving
    ( PMatch
    , PIsData
    )
    via ( PIsDataReprInstances
            (PUpperBound a)
        )
  deriving (PDataFields) via (DerivePDataFields (PUpperBound a))

instance PIsDataRepr (PUpperBound a) where
  type
    PIsDataReprRepr (PUpperBound a) =
      '[ '[ "_0" ':= PExtended a
          , "_1" ':= PClosure
          ]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PUpperBound) DRHNil

data PExtended a (s :: S)
  = PNegInf (Term s (PDataRecord '[]))
  | PFinite (Term s (PDataRecord '["_0" ':= a]))
  | PPosInf (Term s (PDataRecord '[]))
  deriving
    ( PMatch
    , PIsData
    )
    via ( PIsDataReprInstances
            (PExtended a)
        )
  deriving (PDataFields) via (DerivePDataFields (PExtended a))

instance PIsDataRepr (PExtended a) where
  type
    PIsDataReprRepr (PExtended a) =
      '[ '[], '["_0" ':= a], '[]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNegInf) $
        DRHCons (f . PFinite) $
          DRHCons (f . PPosInf) DRHNil

---------- Tx/Address

data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PValidatorHash]))
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PCredential)
  deriving (PDataFields) via (DerivePDataFields PCredential)

instance PIsDataRepr PCredential where
  type
    PIsDataReprRepr PCredential =
      '[ '["_0" ':= PPubKeyHash]
       , '["_0" ':= PValidatorHash]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PPubKeyCredential) $
        DRHCons
          (f . PScriptCredential)
          DRHNil

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
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances PStakingCredential
  deriving (PDataFields) via (DerivePDataFields PStakingCredential)

instance PIsDataRepr PStakingCredential where
  type
    PIsDataReprRepr PStakingCredential =
      '[ '["_0" ':= PCredential]
       , '[ "_0" ':= PInteger
          , "_1" ':= PInteger
          , "_2" ':= PInteger
          ]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PStakingHash) $
        DRHCons (f . PStakingPtr) DRHNil

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
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PAddress
  deriving (PDataFields) via (DerivePDataFields PAddress)

instance PIsDataRepr PAddress where
  type
    PIsDataReprRepr PAddress =
      '[ '[ "credential" ':= PCredential
          , "stakingCredential" ':= (PMaybe PStakingCredential)
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PAddress) DRHNil

---------- Tx

newtype PTxId (s :: S)
  = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PTxId
  deriving (PDataFields) via (DerivePDataFields PTxId)

instance PIsDataRepr PTxId where
  type PIsDataReprRepr PTxId = '[ '["_0" ':= PByteString]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxId) DRHNil

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
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PTxOutRef
  deriving (PDataFields) via (DerivePDataFields PTxOutRef)

instance PIsDataRepr PTxOutRef where
  type
    PIsDataReprRepr PTxOutRef =
      '[ '[ "id" ':= PTxId
          , "idx" ':= PInteger
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOutRef) DRHNil

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
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances PTxInInfo
  deriving (PDataFields) via (DerivePDataFields PTxInInfo)

instance PIsDataRepr PTxInInfo where
  type
    PIsDataReprRepr PTxInInfo =
      '[ '[ "outRef" ':= PTxOutRef
          , "resolved" ':= PTxOut
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxInInfo) DRHNil

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
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PTxOut)
  deriving (PDataFields) via (DerivePDataFields PTxOut)

instance PIsDataRepr PTxOut where
  type
    PIsDataReprRepr PTxOut =
      '[ '[ "address" ':= PAddress
          , "value" ':= PValue
          , "datumHash" ':= PMaybe PDatumHash
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOut) DRHNil

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
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PDCert)

instance PIsDataRepr PDCert where
  type
    PIsDataReprRepr PDCert =
      '[ '["_0" ':= PStakingCredential]
       , '["_0" ':= PStakingCredential]
       , '[ "_0" ':= PStakingCredential
          , "_1" ':= PPubKeyHash
          ]
       , '[ "_0" ':= PPubKeyHash
          , "_1" ':= PPubKeyHash
          ]
       , '[ "_0" ':= PPubKeyHash
          , "_1" ':= PInteger
          ]
       , '[]
       , '[]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PDCertDelegRegKey) $
        DRHCons (f . PDCertDelegDeRegKey) $
          DRHCons (f . PDCertDelegDelegate) $
            DRHCons (f . PDCertPoolRegister) $
              DRHCons (f . PDCertPoolRetire) $
                DRHCons (f . PDCertGenesis) $
                  DRHCons (f . PDCertMir) DRHNil

---------- AssocMap

newtype PMap (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving (PlutusType, PIsData) via (DerivePNewtype (PMap k v) (PBuiltinMap k v))

-- FIXME: This fails typecheck. "Illegal type synonym family application ‘PLifted k’ in instance"
-- instance PUnsafeLiftDecl (PMap k v) where type PLifted (PMap k v) = PlutusMap.Map (PLifted k) (PLifted v)
-- deriving via
--   (DerivePConstantViaNewtype (PlutusMap.Map (PLifted k) (PLifted v)) (PMap k v) (PBuiltinMap k v))
--   instance
--     (PConstant (PlutusMap.Map (PLifted k) (PLifted v)))

---------- Others

data PMaybe a (s :: S)
  = PNothing (Term s (PDataRecord '[]))
  | PJust (Term s (PDataRecord '["_0" ':= a]))
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances
          (PMaybe a)
  deriving (PDataFields) via (DerivePDataFields (PMaybe a))

instance PIsDataRepr (PMaybe a) where
  type PIsDataReprRepr (PMaybe a) = '[ '[], '["_0" ':= a]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNothing) $
        DRHCons (f . PJust) DRHNil

data PEither a b (s :: S)
  = PLeft (Term s (PDataRecord '["_0" ':= a]))
  | PRight (Term s (PDataRecord '["_0" ':= b]))
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances
          (PEither a b)
  deriving (PDataFields) via (DerivePDataFields (PEither a b))

instance PIsDataRepr (PEither a b) where
  type
    PIsDataReprRepr (PEither a b) =
      '[ '["_0" ':= a], '["_0" ':= b]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLeft) $
        DRHCons (f . PRight) DRHNil
