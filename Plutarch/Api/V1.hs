{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.Void
import Plutarch (PMatch, PlutusType, punsafeCoerce)
import Plutarch.Bool (PBool, PEq, POrd)
import Plutarch.Builtin (PData, PIsData, PList, type PBuiltinMap)
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr (
  DataReprHandlers (DRHCons, DRHNil),
  PDataList,
  PIsDataRepr,
  PIsDataReprInstances (PIsDataReprInstances),
  PIsDataReprRepr,
  PVia (..),
  pmatchDataRepr,
  pmatchRepr,
 )
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Lift (AsDefaultUni, PBuiltinType, PHaskellType, PLift (..), pconstant)

-- ctor in-scope for deriving
import Plutarch.Prelude
import Plutarch.Tuple (PTuple)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as Plutus
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import qualified PlutusTx.Prelude as PlutusTx

--------------------------------------------------------------------------------

---------- V1 Specific types, Incompatible with V2

newtype PTxInfo (s :: k)
  = PTxInfo
      ( Term
          s
          ( PDataList
              '[ PList PTxInInfo
               , PList PTxOut
               , PValue
               , PValue
               , PList PDCert
               , PList (PTuple '[PStakingCredential, PInteger])
               , PPOSIXTimeRange
               , PList PPubKeyHash
               , PList (PTuple '[PDatumHash, PDatum])
               , PTxId
               ]
          )
      )
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via PIsDataReprInstances PTxInfo Plutus.TxInfo

instance PIsDataRepr PTxInfo where
  type
    PIsDataReprRepr PTxInfo =
      '[ '[ PList PTxInInfo
          , PList PTxOut
          , PValue
          , PValue
          , PList PDCert
          , PList (PTuple '[PStakingCredential, PInteger])
          , PPOSIXTimeRange
          , PList PPubKeyHash
          , PList (PTuple '[PDatumHash, PDatum])
          , PTxId
          ]
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PTxInfo)) $ DRHNil)

newtype PScriptContext (s :: k)
  = PScriptContext (Term s (PDataList '[PTxInfo, PScriptPurpose]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via PIsDataReprInstances PScriptContext Plutus.ScriptContext

instance PIsDataRepr PScriptContext where
  type
    PIsDataReprRepr PScriptContext =
      '[ '[ PTxInfo
          , PScriptPurpose
          ]
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PScriptContext)) $ DRHNil)

-- General types, used by V1 and V2

data PScriptPurpose (s :: k)
  = PMinting (Term s (PDataList '[PCurrencySymbol]))
  | PSpending (Term s (PDataList '[PTxOutRef]))
  | PRewarding (Term s (PDataList '[PStakingCredential]))
  | PCertifying (Term s (PDataList '[PDCert]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via (PIsDataReprInstances PScriptPurpose Plutus.ScriptPurpose)

instance PIsDataRepr PScriptPurpose where
  type
    PIsDataReprRepr PScriptPurpose =
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

newtype PDatum (s :: k) = PDatum (Term s PData)
  deriving (PIsData, PEq) via PData
  deriving (PLift, PlutusType, AsDefaultUni) via (PVia PData Plutus.Datum)

newtype PRedeemer (s :: k) = PRedeemer (Term s PData)
  deriving (PIsData, PEq) via PData
  deriving (PLift, PlutusType, AsDefaultUni) via (PVia PData Plutus.Redeemer)

newtype PDatumHash (s :: k)
  = PDatumHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving (PLift, PlutusType, AsDefaultUni) via (PVia PByteString Plutus.DatumHash)

newtype PStakeValidatorHash (s :: k)
  = PStakeValidatorHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.StakeValidatorHash)

newtype PRedeemerHash (s :: k)
  = PRedeemerHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.RedeemerHash)

newtype PValidatorHash (s :: k)
  = PValidatorHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.ValidatorHash)

---------- Value

newtype PTokenName (s :: k)
  = PTokenName (Term s PByteString)
  deriving (PEq, POrd, PIsData) via (PByteString)
  deriving newtype (Semigroup, Monoid)
  deriving (PLift, PlutusType, AsDefaultUni) via (PVia PByteString Plutus.TokenName)

newtype PValue (s :: k)
  = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving (PIsData) via (PMap PCurrencySymbol (PMap PTokenName PInteger))
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via PVia
          (PMap PCurrencySymbol (PMap PTokenName PInteger))
          Plutus.Value

newtype PCurrencySymbol (s :: k)
  = PCurrencySymbol (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.CurrencySymbol)

---------- Crypto

newtype PPubKeyHash (s :: k)
  = PPubKeyHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.PubKeyHash)

newtype PPubKey (s :: k)
  = PPubKey (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.PubKey)

newtype PSignature (s :: k)
  = PSignature (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString
  deriving
    (PLift, PlutusType, AsDefaultUni)
    via (PVia PByteString Plutus.Signature)

---------- Time

newtype PPOSIXTime (s :: k)
  = PPOSIXTime (Term s PInteger)
  deriving (POrd, PEq, PIntegral, PIsData) via (PInteger)
  deriving newtype (Num)
  deriving (PLift, PlutusType, AsDefaultUni) via (PVia PInteger Plutus.POSIXTime)

type PPOSIXTimeRange = PInterval PPOSIXTime

---------- Interval

type PClosure = PBool

newtype PInterval a (s :: k)
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
    ( PMatch
    , PIsData
    , AsDefaultUni
    )
    via PIsDataReprInstances
          (PInterval a)
          (Plutus.Interval (PHaskellType a))

deriving via
  PIsDataReprInstances
    (PInterval a)
    (Plutus.Interval (PHaskellType a))
  instance
    ( Plutus.FromData ah
    , Plutus.ToData ah
    , (PHaskellType a ~ ah)
    ) =>
    (PLift (PInterval a))

instance PIsDataRepr (PInterval a) where
  type
    PIsDataReprRepr (PInterval a) =
      '[ '[ PLowerBound a
          , PUpperBound a
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PInterval) DRHNil

newtype PLowerBound a (s :: k)
  = PLowerBound (Term s (PDataList '[PExtended a, PClosure]))
  deriving
    ( PMatch
    , PIsData
    , AsDefaultUni
    )
    via ( PIsDataReprInstances
            (PLowerBound a)
            (Plutus.LowerBound (PHaskellType a))
        )

deriving via
  PIsDataReprInstances
    (PLowerBound a)
    (Plutus.LowerBound (PHaskellType a))
  instance
    ( Plutus.FromData ah
    , Plutus.ToData ah
    , (PHaskellType a ~ ah)
    ) =>
    (PLift (PLowerBound a))

instance PIsDataRepr (PLowerBound a) where
  type
    PIsDataReprRepr (PLowerBound a) =
      '[ '[ PExtended a
          , PClosure
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLowerBound) DRHNil

newtype PUpperBound a (s :: k)
  = PUpperBound (Term s (PDataList '[PExtended a, PClosure]))
  deriving
    ( PMatch
    , PIsData
    , AsDefaultUni
    )
    via ( PIsDataReprInstances
            (PUpperBound a)
            (Plutus.UpperBound (PHaskellType a))
        )

deriving via
  PIsDataReprInstances
    (PUpperBound a)
    (Plutus.UpperBound (PHaskellType a))
  instance
    ( Plutus.FromData ah
    , Plutus.ToData ah
    , (PHaskellType a ~ ah)
    ) =>
    (PLift (PUpperBound a))

instance PIsDataRepr (PUpperBound a) where
  type
    PIsDataReprRepr (PUpperBound a) =
      '[ '[ PExtended a
          , PClosure
          ]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PUpperBound) DRHNil

data PExtended a (s :: k)
  = PNegInf (Term s (PDataList '[]))
  | PFinite (Term s (PDataList '[a]))
  | PPosInf (Term s (PDataList '[]))
  deriving
    ( PMatch
    , PIsData
    , AsDefaultUni
    )
    via ( PIsDataReprInstances
            (PExtended a)
            (Plutus.Extended (PHaskellType a))
        )

deriving via
  PIsDataReprInstances
    (PExtended a)
    (Plutus.Extended (PHaskellType a))
  instance
    ( Plutus.FromData ah
    , Plutus.ToData ah
    , (PHaskellType a ~ ah)
    ) =>
    (PLift (PExtended a))

instance PIsDataRepr (PExtended a) where
  type PIsDataReprRepr (PExtended a) = '[ '[], '[a], '[]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNegInf) $
        DRHCons (f . PFinite) $
          DRHCons (f . PPosInf) DRHNil

---------- Tx/Address

data PCredential (s :: k)
  = PPubKeyCredential (Term s (PDataList '[PPubKeyHash]))
  | PScriptCredential (Term s (PDataList '[PValidatorHash]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via (PIsDataReprInstances PCredential Plutus.Credential)

instance PIsDataRepr PCredential where
  type
    PIsDataReprRepr PCredential =
      '[ '[PPubKeyHash]
       , '[PValidatorHash]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PPubKeyCredential) $
        DRHCons
          (f . PScriptCredential)
          DRHNil

data PStakingCredential (s :: k)
  = PStakingHash (Term s (PDataList '[PCredential]))
  | PStakingPtr (Term s (PDataList '[PInteger, PInteger, PInteger]))
  deriving
    ( PMatch
    , PIsData
    , PLift
    , AsDefaultUni
    )
    via PIsDataReprInstances PStakingCredential Plutus.StakingCredential

instance PIsDataRepr PStakingCredential where
  type
    PIsDataReprRepr PStakingCredential =
      '[ '[PCredential]
       , '[PInteger, PInteger, PInteger]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PStakingHash) $
        DRHCons (f . PStakingPtr) DRHNil

newtype PAddress (s :: k)
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
    (PMatch, PIsData, PLift, AsDefaultUni)
    via PIsDataReprInstances PAddress Plutus.Address

instance PIsDataRepr PAddress where
  type
    PIsDataReprRepr PAddress =
      '[ '[ PCredential
          , PMaybe PStakingCredential
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PAddress) DRHNil

---------- Tx

newtype PTxId (s :: k)
  = PTxId (Term s (PDataList '[PByteString]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via PIsDataReprInstances PTxId Plutus.TxId

instance PIsDataRepr PTxId where
  type PIsDataReprRepr PTxId = '[ '[PByteString]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxId) DRHNil

newtype PTxOutRef (s :: k)
  = PTxOutRef (Term s (PDataList '[PTxId, PInteger]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via PIsDataReprInstances PTxOutRef Plutus.TxOutRef

instance PIsDataRepr PTxOutRef where
  type PIsDataReprRepr PTxOutRef = '[ '[PTxId, PInteger]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOutRef) DRHNil

newtype PTxInInfo (s :: k)
  = PTxInInfo (Term s (PDataList '[PTxOutRef, PTxOut]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via PIsDataReprInstances PTxInInfo Plutus.TxInfo

instance PIsDataRepr PTxInInfo where
  type PIsDataReprRepr PTxInInfo = '[ '[PTxOutRef, PTxOut]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxInInfo) DRHNil

newtype PTxOut (s :: k)
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
    (PMatch, PIsData, PLift, AsDefaultUni)
    via (PIsDataReprInstances PTxOut Plutus.TxOut)

instance PIsDataRepr PTxOut where
  type
    PIsDataReprRepr PTxOut =
      '[ '[ PAddress
          , PValue
          , PMaybe PDatumHash
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOut) DRHNil

data PDCert (s :: k)
  = PDCertDelegRegKey (Term s (PDataList '[PStakingCredential]))
  | PDCertDelegDeRegKey (Term s (PDataList '[PStakingCredential]))
  | PDCertDelegDelegate (Term s (PDataList '[PStakingCredential, PPubKeyHash]))
  | PDCertPoolRegister (Term s (PDataList '[PPubKeyHash, PPubKeyHash]))
  | PDCertPoolRetire (Term s (PDataList '[PPubKeyHash, PInteger]))
  | PDCertGenesis (Term s (PDataList '[]))
  | PDCertMir (Term s (PDataList '[]))
  deriving
    (PMatch, PIsData, PLift, AsDefaultUni)
    via (PIsDataReprInstances PDCert (Plutus.DCert))

instance PIsDataRepr PDCert where
  type
    PIsDataReprRepr PDCert =
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

newtype PMap (a :: k -> Type) (b :: k -> Type) (s :: k)
  = PMap (Term s (PBuiltinMap a b))
  deriving (PIsData) via (PBuiltinMap a b)
  deriving (PlutusType, AsDefaultUni) via PVia (PBuiltinMap a b) Void

-- AssocMap.Map isn't exported, or we could use PVia
instance
  ( Plutus.FromData (PHaskellType k)
  , Plutus.FromData (PHaskellType v)
  , Plutus.ToData (PHaskellType k)
  , Plutus.ToData (PHaskellType v)
  ) =>
  PLift (PMap k v)
  where
  type
    PHaskellType (PMap k v) =
      AssocMap.Map (PHaskellType k) (PHaskellType v)

  pconstant' =
    punsafeCoerce
      . pconstant
        @(PBuiltinType (PBuiltinMap k v) [((PHaskellType k), (PHaskellType v))])
      . AssocMap.toList

  plift' t = do
    let t' :: Term s (PBuiltinType (PBuiltinMap k v) [(PHaskellType k, PHaskellType v)])
        t' = punsafeCoerce t

    AssocMap.fromList <$> plift' t'

---------- Others

data PMaybe a (s :: k)
  = PNothing (Term s (PDataList '[]))
  | PJust (Term s (PDataList '[a]))
  deriving
    (PMatch, PIsData, AsDefaultUni)
    via PIsDataReprInstances
          (PMaybe a)
          (PlutusTx.Maybe (PHaskellType a))

deriving via
  PIsDataReprInstances
    (PMaybe a)
    (PlutusTx.Maybe (PHaskellType a))
  instance
    ( Plutus.FromData ah
    , Plutus.ToData ah
    , (PHaskellType a ~ ah)
    ) =>
    (PLift (PMaybe a))

instance PIsDataRepr (PMaybe a) where
  type PIsDataReprRepr (PMaybe a) = '[ '[], '[a]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNothing) $
        DRHCons (f . PJust) DRHNil

data PEither a b (s :: k)
  = PLeft (Term s (PDataList '[a]))
  | PRight (Term s (PDataList '[b]))
  deriving
    ( PMatch
    , PIsData
    , AsDefaultUni
    )
    via PIsDataReprInstances
          (PEither a b)
          (PlutusTx.Either (PHaskellType a) (PHaskellType b))

deriving via
  PIsDataReprInstances
    (PEither a b)
    (PlutusTx.Either (PHaskellType a) (PHaskellType b))
  instance
    ( Plutus.FromData ah
    , Plutus.FromData bh
    , Plutus.ToData ah
    , Plutus.ToData bh
    , (PHaskellType a ~ ah)
    , (PHaskellType b ~ bh)
    ) =>
    (PLift (PEither a b))

instance PIsDataRepr (PEither a b) where
  type
    PIsDataReprRepr (PEither a b) =
      '[ '[a], '[b]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLeft) $
        DRHCons (f . PRight) DRHNil
