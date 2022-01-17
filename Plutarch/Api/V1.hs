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

import Plutarch (PMatch)
import Plutarch.Bool (PBool, PEq, POrd)
import Plutarch.Builtin (PAsData, PBuiltinList, PData, PIsData, type PBuiltinMap)
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr (
  DataReprHandlers (DRHCons, DRHNil),
  PDataList,
  PIsDataRepr,
  PIsDataReprInstances (PIsDataReprInstances),
  PIsDataReprRepr,
  pmatchDataRepr,
  pmatchRepr,
 )
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)

-- ctor in-scope for deriving
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.Prelude as PlutusTx

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxInfo)
    via PIsDataReprInstances PTxInfo Plutus.TxInfo

instance PIsDataRepr PTxInfo where
  type
    PIsDataReprRepr PTxInfo =
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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.ScriptContext)
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

data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataList '[PCurrencySymbol]))
  | PSpending (Term s (PDataList '[PTxOutRef]))
  | PRewarding (Term s (PDataList '[PStakingCredential]))
  | PCertifying (Term s (PDataList '[PDCert]))
  deriving
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.ScriptPurpose)
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

newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving (PIsData, PEq) via PData

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving (PIsData, PEq) via PData

newtype PDatumHash (s :: S)
  = PDatumHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

newtype PStakeValidatorHash (s :: S)
  = PStakeValidatorHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

newtype PRedeemerHash (s :: S)
  = PRedeemerHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

newtype PValidatorHash (s :: S)
  = PValidatorHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

---------- Value

newtype PTokenName (s :: S)
  = PTokenName (Term s PByteString)
  deriving (PEq, POrd, PIsData) via (PByteString)
  deriving newtype (Semigroup, Monoid)

newtype PValue (s :: S)
  = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving (PIsData) via (PMap PCurrencySymbol (PMap PTokenName PInteger))

newtype PCurrencySymbol (s :: S)
  = PCurrencySymbol (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

---------- Crypto

newtype PPubKeyHash (s :: S)
  = PPubKeyHash (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

newtype PPubKey (s :: S)
  = PPubKey (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

newtype PSignature (s :: S)
  = PSignature (Term s PByteString)
  deriving (PEq, POrd, PIsData) via PByteString

---------- Time

newtype PPOSIXTime (s :: S)
  = PPOSIXTime (Term s PInteger)
  deriving (POrd, PEq, PIntegral, PIsData) via (PInteger)
  deriving newtype (Num)

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
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances
          (PInterval a)
          (Plutus.Interval (PLifted a))

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

newtype PLowerBound a (s :: S)
  = PLowerBound (Term s (PDataList '[PExtended a, PClosure]))
  deriving
    ( PMatch
    , PIsData
    )
    via ( PIsDataReprInstances
            (PLowerBound a)
            (Plutus.LowerBound (PLifted a))
        )

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

newtype PUpperBound a (s :: S)
  = PUpperBound (Term s (PDataList '[PExtended a, PClosure]))
  deriving
    ( PMatch
    , PIsData
    )
    via ( PIsDataReprInstances
            (PUpperBound a)
            (Plutus.UpperBound (PLifted a))
        )

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

data PExtended a (s :: S)
  = PNegInf (Term s (PDataList '[]))
  | PFinite (Term s (PDataList '[a]))
  | PPosInf (Term s (PDataList '[]))
  deriving
    ( PMatch
    , PIsData
    )
    via ( PIsDataReprInstances
            (PExtended a)
            (Plutus.Extended (PLifted a))
        )

instance PIsDataRepr (PExtended a) where
  type PIsDataReprRepr (PExtended a) = '[ '[], '[a], '[]]
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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.Credential)
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

data PStakingCredential (s :: S)
  = PStakingHash (Term s (PDataList '[PCredential]))
  | PStakingPtr (Term s (PDataList '[PInteger, PInteger, PInteger]))
  deriving
    ( PMatch
    , PIsData
    , PUnsafeLiftDecl Plutus.StakingCredential
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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.Address)
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

newtype PTxId (s :: S)
  = PTxId (Term s (PDataList '[PByteString]))
  deriving
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxId)
    via PIsDataReprInstances PTxId Plutus.TxId

instance PIsDataRepr PTxId where
  type PIsDataReprRepr PTxId = '[ '[PByteString]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxId) DRHNil

newtype PTxOutRef (s :: S)
  = PTxOutRef (Term s (PDataList '[PTxId, PInteger]))
  deriving
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxOutRef)
    via PIsDataReprInstances PTxOutRef Plutus.TxOutRef

instance PIsDataRepr PTxOutRef where
  type PIsDataReprRepr PTxOutRef = '[ '[PTxId, PInteger]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxOutRef) DRHNil

newtype PTxInInfo (s :: S)
  = PTxInInfo (Term s (PDataList '[PTxOutRef, PTxOut]))
  deriving
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxInfo)
    via PIsDataReprInstances PTxInInfo Plutus.TxInfo

instance PIsDataRepr PTxInInfo where
  type PIsDataReprRepr PTxInInfo = '[ '[PTxOutRef, PTxOut]]

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxOut)
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

data PDCert (s :: S)
  = PDCertDelegRegKey (Term s (PDataList '[PStakingCredential]))
  | PDCertDelegDeRegKey (Term s (PDataList '[PStakingCredential]))
  | PDCertDelegDelegate (Term s (PDataList '[PStakingCredential, PPubKeyHash]))
  | PDCertPoolRegister (Term s (PDataList '[PPubKeyHash, PPubKeyHash]))
  | PDCertPoolRetire (Term s (PDataList '[PPubKeyHash, PInteger]))
  | PDCertGenesis (Term s (PDataList '[]))
  | PDCertMir (Term s (PDataList '[]))
  deriving
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.DCert)
    via (PIsDataReprInstances PDCert Plutus.DCert)

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

newtype PMap (a :: PType) (b :: PType) (s :: S)
  = PMap (Term s (PBuiltinMap a b))
  deriving (PIsData) via (PBuiltinMap a b)

---------- Others

data PMaybe a (s :: S)
  = PNothing (Term s (PDataList '[]))
  | PJust (Term s (PDataList '[a]))
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances
          (PMaybe a)
          (PlutusTx.Maybe (PLifted a))

instance PIsDataRepr (PMaybe a) where
  type PIsDataReprRepr (PMaybe a) = '[ '[], '[a]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PNothing) $
        DRHCons (f . PJust) DRHNil

data PEither a b (s :: S)
  = PLeft (Term s (PDataList '[a]))
  | PRight (Term s (PDataList '[b]))
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances
          (PEither a b)
          (PlutusTx.Either (PLifted a) (PLifted b))

instance PIsDataRepr (PEither a b) where
  type
    PIsDataReprRepr (PEither a b) =
      '[ '[a], '[b]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLeft) $
        DRHCons (f . PRight) DRHNil
