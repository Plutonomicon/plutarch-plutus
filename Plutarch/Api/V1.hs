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
import Plutarch.DataRepr
    ( DerivePConstantViaData(..),
      PIsDataRepr(..),
      PIsDataReprInstances(..),
      DataReprHandlers(DRHNil, DRHCons),
      PDataList,
      pmatchDataRepr ) 
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype), PConstant, PLifted, PUnsafeLiftDecl)

-- ctor in-scope for deriving
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as PlutusCrpyto
import qualified PlutusTx.Builtins.Internal as PT

--------------------------------------------------------------------------------

type PTuple = PDataList

---------- V1 Specific types, Incompatible with V2

newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataList
              '[ PBuiltinList (PAsData PTxInInfo)
               , PBuiltinList (PAsData PTxOut)
               , PValue
               , PValue
               , PBuiltinList (PAsData PDCert)
               , PBuiltinList (PAsData (PTuple '[PStakingCredential, PInteger]))
               , PPOSIXTimeRange
               , PBuiltinList (PAsData PPubKeyHash)
               , PBuiltinList (PAsData (PTuple '[PDatumHash, PDatum]))
               , PTxId
               ]
          )
      )
  deriving
    (PIsData)
    via PIsDataReprInstances PTxInfo

deriving via (PIsDataReprInstances PTxInfo) instance (PMatch s PTxInfo)
instance PUnsafeLiftDecl PTxInfo where type PLifted PTxInfo = Plutus.TxInfo
deriving via (DerivePConstantViaData Plutus.TxInfo PTxInfo) instance (PConstant Plutus.TxInfo)

instance PIsDataRepr s PTxInfo where
  type
    PIsDataReprRepr s PTxInfo =
      '[ '[ PBuiltinList (PAsData PTxInInfo)
          , PBuiltinList (PAsData PTxOut)
          , PValue
          , PValue
          , PBuiltinList (PAsData PDCert)
          , PBuiltinList (PAsData (PTuple '[PStakingCredential, PInteger]))
          , PPOSIXTimeRange
          , PBuiltinList (PAsData PPubKeyHash)
          , PBuiltinList (PAsData (PTuple '[PDatumHash, PDatum]))
          , PTxId
          ]
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PTxInfo)) $ DRHNil)

newtype PScriptContext (s :: S)
  = PScriptContext (Term s (PDataList '[PTxInfo, PScriptPurpose]))
  deriving
    (PIsData)
    via PIsDataReprInstances PScriptContext

deriving via (PIsDataReprInstances PScriptContext) instance (PMatch s PScriptContext)
instance PUnsafeLiftDecl PScriptContext where type PLifted PScriptContext = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance (PConstant Plutus.ScriptContext)

instance PIsDataRepr s PScriptContext where
  type
    PIsDataReprRepr s PScriptContext =
      '[ '[ PTxInfo
          , PScriptPurpose
          ]
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PScriptContext)) $ DRHNil)

-- General types, used by V1 and V2

data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataList '[PCurrencySymbol]))
  | PSpending (Term s (PDataList '[PTxOutRef]))
  | PRewarding (Term s (PDataList '[PStakingCredential]))
  | PCertifying (Term s (PDataList '[PDCert]))
  deriving
    (PIsData)
    via (PIsDataReprInstances PScriptPurpose)

deriving via (PIsDataReprInstances PScriptPurpose) instance (PMatch s PScriptPurpose)
instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
deriving via (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) instance (PConstant Plutus.ScriptPurpose)

instance PIsDataRepr s PScriptPurpose where
  type
    PIsDataReprRepr s PScriptPurpose =
      '[ '[PCurrencySymbol]
       , '[PTxOutRef]
       , '[PStakingCredential]
       , '[PDCert]
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
  deriving (PIsData) via (DerivePNewtype PDatum PData)

deriving via (DerivePNewtype PDatum PData) instance (PlutusType s PDatum)
instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaNewtype Plutus.Datum PDatum PData) instance (PConstant Plutus.Datum)

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving (PIsData) via (DerivePNewtype PRedeemer PData)

deriving via (DerivePNewtype PRedeemer PData) instance (PlutusType s PRedeemer )
instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaNewtype Plutus.Redeemer PRedeemer PData) instance (PConstant Plutus.Redeemer)

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PDatumHash PByteString)

deriving via (DerivePNewtype PDatumHash PByteString) instance (PlutusType s PDatumHash)
instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaNewtype Plutus.DatumHash PDatumHash PByteString) instance (PConstant Plutus.DatumHash)

newtype PStakeValidatorHash (s :: S) = PStakeValidatorHash (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PStakeValidatorHash PByteString)

deriving via (DerivePNewtype PStakeValidatorHash PByteString) instance (PlutusType s PStakeValidatorHash)
instance PUnsafeLiftDecl PStakeValidatorHash where type PLifted PStakeValidatorHash = Plutus.StakeValidatorHash
deriving via (DerivePConstantViaNewtype Plutus.StakeValidatorHash PStakeValidatorHash PByteString) instance (PConstant Plutus.StakeValidatorHash)

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PRedeemerHash PByteString)

deriving via (DerivePNewtype PRedeemerHash PByteString) instance (PlutusType s PRedeemerHash)
instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via (DerivePConstantViaNewtype Plutus.RedeemerHash PRedeemerHash PByteString) instance (PConstant Plutus.RedeemerHash)

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PValidatorHash PByteString)

deriving via (DerivePNewtype PValidatorHash PByteString) instance (PlutusType s PValidatorHash)
instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
deriving via (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString) instance (PConstant Plutus.ValidatorHash)

---------- Value

newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving newtype (Semigroup, Monoid)
  deriving (PIsData) via (DerivePNewtype PTokenName PByteString)

deriving via (DerivePNewtype PTokenName PByteString) instance (PlutusType s PTokenName)
instance PUnsafeLiftDecl PTokenName where type PLifted PTokenName = Plutus.TokenName
deriving via
  (DerivePConstantViaNewtype Plutus.TokenName PTokenName PByteString)
  instance
    (PConstant Plutus.TokenName)

newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PCurrencySymbol PByteString)

deriving via (DerivePNewtype PCurrencySymbol PByteString) instance (PlutusType s PCurrencySymbol)
instance PUnsafeLiftDecl PCurrencySymbol where type PLifted PCurrencySymbol = Plutus.CurrencySymbol
deriving via
  (DerivePConstantViaNewtype Plutus.CurrencySymbol PCurrencySymbol PByteString)
  instance
    (PConstant Plutus.CurrencySymbol)

newtype PValue (s :: S) = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    ( 
    -- FIXME: This requires `PIsData` instance for `PMap`.
    -- , PIsData
    )
    via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))

deriving via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))  instance (PlutusType s PValue)
-- FIXME: This fails typecheck.
-- instance PUnsafeLiftDecl PValue where type PLifted PValue = Plutus.Value
-- deriving via
--   (DerivePConstantViaNewtype Plutus.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
--   instance
--     (PConstant Plutus.Value)

---------- Crypto

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PPubKeyHash PByteString)

deriving via (DerivePNewtype PPubKeyHash PByteString) instance (PlutusType s PPubKeyHash)
instance PUnsafeLiftDecl PPubKeyHash where type PLifted PPubKeyHash = Plutus.PubKeyHash
deriving via
  (DerivePConstantViaNewtype Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    (PConstant Plutus.PubKeyHash)

newtype PPubKey (s :: S) = PPubKey (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PPubKey PByteString)

deriving via (DerivePNewtype PPubKey PByteString) instance (PlutusType s PPubKey)
instance PUnsafeLiftDecl PPubKey where type PLifted PPubKey = PlutusCrpyto.PubKey
deriving via
  (DerivePConstantViaNewtype PlutusCrpyto.PubKey PPubKey PByteString)
  instance
    (PConstant PlutusCrpyto.PubKey)

newtype PSignature (s :: S) = PSignature (Term s PByteString)
  deriving (PIsData) via (DerivePNewtype PSignature PByteString)

deriving via (DerivePNewtype PSignature PByteString) instance (PlutusType s PSignature)
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
  deriving (PIsData) via (DerivePNewtype PPOSIXTime PInteger)

deriving via (DerivePNewtype PPOSIXTime PInteger) instance (PlutusType s PPOSIXTime)
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
          ( PDataList
              '[ PLowerBound a
               , PUpperBound a
               ]
          )
      )
  deriving
    ( PIsData
    )
    via PIsDataReprInstances
          (PInterval a)

deriving via PIsDataReprInstances (PInterval a) instance (PMatch s (PInterval a))

instance PIsDataRepr s (PInterval a) where
  type
    PIsDataReprRepr s (PInterval a) =
      '[ '[ PLowerBound a
          , PUpperBound a
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PInterval) DRHNil

newtype PLowerBound a (s :: S)
  = PLowerBound (Term s (PDataList '[PExtended a, PClosure]))
  deriving
    ( PIsData
    )
    via ( PIsDataReprInstances
            (PLowerBound a)
        )

deriving via PIsDataReprInstances (PLowerBound a) instance (PMatch s (PLowerBound a))

instance PIsDataRepr s (PLowerBound a) where
  type
    PIsDataReprRepr s (PLowerBound a) =
      '[ '[ PExtended a
          , PClosure
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLowerBound) DRHNil

newtype PUpperBound a (s :: S)
  = PUpperBound (Term s (PDataList '[PExtended a, PClosure]))
  deriving
    ( PIsData
    )
    via ( PIsDataReprInstances
            (PUpperBound a)
        )

deriving via PIsDataReprInstances (PUpperBound a) instance (PMatch s (PUpperBound a))

instance PIsDataRepr s (PUpperBound a) where
  type
    PIsDataReprRepr s (PUpperBound a) =
      '[ '[ PExtended a
          , PClosure
          ]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PUpperBound) DRHNil

data PExtended a (s :: S)
  = PNegInf (Term s (PDataList '[]))
  | PFinite (Term s (PDataList '[a]))
  | PPosInf (Term s (PDataList '[]))
  deriving
    ( PIsData
    )
    via ( PIsDataReprInstances
            (PExtended a)
        )

deriving via (PIsDataReprInstances (PExtended a)) instance (PMatch s (PExtended a))
instance PIsDataRepr s (PExtended a) where
  type PIsDataReprRepr s (PExtended a) = '[ '[], '[a], '[]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNegInf) $
        DRHCons (f . PFinite) $
          DRHCons (f . PPosInf) DRHNil

---------- Tx/Address

data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataList '[PPubKeyHash]))
  | PScriptCredential (Term s (PDataList '[PValidatorHash]))
  deriving
    (PIsData)
    via (PIsDataReprInstances PCredential)

deriving via (PIsDataReprInstances PCredential) instance (PMatch s PCredential)

instance PIsDataRepr s PCredential where
  type
    PIsDataReprRepr s PCredential =
      '[ '[PPubKeyHash]
       , '[PValidatorHash]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PPubKeyCredential) $
        DRHCons
          (f . PScriptCredential)
          DRHNil

data PStakingCredential (s :: S)
  = PStakingHash (Term s (PDataList '[PCredential]))
  | PStakingPtr (Term s (PDataList '[PInteger, PInteger, PInteger]))
  deriving
    ( PIsData
    )
    via PIsDataReprInstances PStakingCredential

deriving via (PIsDataReprInstances PStakingCredential) instance (PMatch s PStakingCredential)


instance PIsDataRepr s PStakingCredential where
  type
    PIsDataReprRepr s PStakingCredential =
      '[ '[PCredential]
       , '[PInteger, PInteger, PInteger]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PStakingHash) $
        DRHCons (f . PStakingPtr) DRHNil

newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataList
              '[ PCredential
               , PMaybe PStakingCredential
               ]
          )
      )
  deriving
    (PIsData)
    via PIsDataReprInstances PAddress

deriving via (PIsDataReprInstances PAddress) instance (PMatch s PAddress)

instance PIsDataRepr s PAddress where
  type
    PIsDataReprRepr s PAddress =
      '[ '[ PCredential
          , PMaybe PStakingCredential
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PAddress) DRHNil

---------- Tx

newtype PTxId (s :: S)
  = PTxId (Term s (PDataList '[PByteString]))
  deriving
    (PIsData)
    via PIsDataReprInstances PTxId

deriving via (PIsDataReprInstances PTxId) instance (PMatch s PTxId)

instance PIsDataRepr s PTxId where
  type PIsDataReprRepr s PTxId = '[ '[PByteString]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxId) DRHNil

newtype PTxOutRef (s :: S)
  = PTxOutRef (Term s (PDataList '[PTxId, PInteger]))
  deriving
    (PIsData)
    via PIsDataReprInstances PTxOutRef

deriving via (PIsDataReprInstances PTxOutRef) instance (PMatch s PTxOutRef)


instance PIsDataRepr s PTxOutRef where
  type PIsDataReprRepr s PTxOutRef = '[ '[PTxId, PInteger]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOutRef) DRHNil

newtype PTxInInfo (s :: S)
  = PTxInInfo (Term s (PDataList '[PTxOutRef, PTxOut]))
  deriving
    (PIsData)
    via PIsDataReprInstances PTxInInfo

deriving via (PIsDataReprInstances PTxInInfo) instance (PMatch s PTxInInfo)


instance PIsDataRepr s PTxInInfo where
  type PIsDataReprRepr s PTxInInfo = '[ '[PTxOutRef, PTxOut]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxInInfo) DRHNil

newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataList
              '[ PAddress
               , PValue
               , PMaybe PDatumHash
               ]
          )
      )
  deriving
    (PIsData)
    via (PIsDataReprInstances PTxOut)

deriving via (PIsDataReprInstances PTxOut) instance (PMatch s PTxOut)


instance PIsDataRepr s PTxOut where
  type
    PIsDataReprRepr s PTxOut =
      '[ '[ PAddress
          , PValue
          , PMaybe PDatumHash
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOut) DRHNil

data PDCert (s :: S)
  = PDCertDelegRegKey (Term s (PDataList '[PStakingCredential]))
  | PDCertDelegDeRegKey (Term s (PDataList '[PStakingCredential]))
  | PDCertDelegDelegate (Term s (PDataList '[PStakingCredential, PPubKeyHash]))
  | PDCertPoolRegister (Term s (PDataList '[PPubKeyHash, PPubKeyHash]))
  | PDCertPoolRetire (Term s (PDataList '[PPubKeyHash, PInteger]))
  | PDCertGenesis (Term s (PDataList '[]))
  | PDCertMir (Term s (PDataList '[]))
  deriving
    (PIsData)
    via (PIsDataReprInstances PDCert)

deriving via (PIsDataReprInstances PDCert) instance (PMatch s PDCert)


instance PIsDataRepr s PDCert where
  type
    PIsDataReprRepr s PDCert =
      '[ '[PStakingCredential]
       , '[PStakingCredential]
       , '[PStakingCredential, PPubKeyHash]
       , '[PPubKeyHash, PPubKeyHash]
       , '[PPubKeyHash, PInteger]
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

newtype PMap (a :: PType) (b :: PType) (s :: S)
  = PMap (Term s (PBuiltinMap a b))

-- TODO: PMap needs instances for PConstant/PLift, PlutusType, and PIsData.

---------- Others

data PMaybe a (s :: S)
  = PNothing (Term s (PDataList '[]))
  | PJust (Term s (PDataList '[a]))
  deriving
    (PIsData)
    via PIsDataReprInstances
          (PMaybe a)

deriving via (PIsDataReprInstances (PMaybe a)) instance (PMatch s (PMaybe a))


instance PIsDataRepr s (PMaybe a) where
  type PIsDataReprRepr s (PMaybe a) = '[ '[], '[a]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNothing) $
        DRHCons (f . PJust) DRHNil

data PEither a b (s :: S)
  = PLeft (Term s (PDataList '[a]))
  | PRight (Term s (PDataList '[b]))
  deriving
    ( PIsData
    )
    via PIsDataReprInstances
          (PEither a b)

deriving via (PIsDataReprInstances (PEither a b)) instance (PMatch s (PEither a b))
instance PIsDataRepr s (PEither a b) where
  type
    PIsDataReprRepr s (PEither a b) =
      '[ '[a], '[b]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLeft) $
        DRHCons (f . PRight) DRHNil
