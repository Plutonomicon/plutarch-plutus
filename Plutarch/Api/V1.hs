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
  PDataRecord,
  PIsDataRepr,
  PIsDataReprInstances (PIsDataReprInstances),
  PIsDataReprRepr,
  PLabeled (..),
  pmatchDataRepr,
  pmatchRepr,
 )
import Plutarch.Field (DerivePDataFields, PDataFields (..))
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)

-- ctor in-scope for deriving
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.Prelude as PlutusTx

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxInfo)
    via PIsDataReprInstances PTxInfo Plutus.TxInfo
  deriving (PDataFields) via (DerivePDataFields PTxInfo)

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.ScriptContext)
    via PIsDataReprInstances PScriptContext Plutus.ScriptContext
  deriving (PDataFields) via (DerivePDataFields PScriptContext)

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.ScriptPurpose)
    via (PIsDataReprInstances PScriptPurpose Plutus.ScriptPurpose)
  deriving (PDataFields) via (DerivePDataFields PScriptPurpose)

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
          (Plutus.Interval (PLifted a))

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
            (Plutus.LowerBound (PLifted a))
        )

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
            (Plutus.UpperBound (PLifted a))
        )

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
            (Plutus.Extended (PLifted a))
        )

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.Credential)
    via (PIsDataReprInstances PCredential Plutus.Credential)
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
    , PUnsafeLiftDecl Plutus.StakingCredential
    )
    via PIsDataReprInstances PStakingCredential Plutus.StakingCredential

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.Address)
    via PIsDataReprInstances PAddress Plutus.Address
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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxId)
    via PIsDataReprInstances PTxId Plutus.TxId

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxOutRef)
    via PIsDataReprInstances PTxOutRef Plutus.TxOutRef

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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxInInfo)
    via PIsDataReprInstances PTxInInfo Plutus.TxInInfo
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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.TxOut)
    via (PIsDataReprInstances PTxOut Plutus.TxOut)
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
    (PMatch, PIsData, PUnsafeLiftDecl Plutus.DCert)
    via (PIsDataReprInstances PDCert Plutus.DCert)

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

newtype PMap (a :: PType) (b :: PType) (s :: S)
  = PMap (Term s (PBuiltinMap a b))
  deriving (PIsData) via (PBuiltinMap a b)

---------- Others

data PMaybe a (s :: S)
  = PNothing (Term s (PDataRecord '[]))
  | PJust (Term s (PDataRecord '["_0" ':= a]))
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances
          (PMaybe a)
          (PlutusTx.Maybe (PLifted a))

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
          (PlutusTx.Either (PLifted a) (PLifted b))

instance PIsDataRepr (PEither a b) where
  type
    PIsDataReprRepr (PEither a b) =
      '[ '["_0" ':= a], '["_0" ':= b]]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PLeft) $
        DRHCons (f . PRight) DRHNil
