{-# LANGUAGE DerivingVia #-}
module Plutarch.Api.V2 
  ( module Plutarch.Api.V1
    -- * V2 specific types
  , PScriptContext (..)
  , PTxInfo (..)
  ) 
  where

--------------------------------------------------------------------------------

import Plutarch (PMatch)
import Plutarch.Builtin (PBuiltinList, PIsData)
import Plutarch.DataRepr 
  (DataReprHandlers (DRHCons, DRHNil), PDataList, PIsDataRepr, PIsDataReprInstances (PIsDataReprInstances), PIsDataReprRepr, pmatchDataRepr, pmatchRepr)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.Api.V1 hiding (PScriptContext (..), PTxInfo (..))

--------------------------------------------------------------------------------

newtype PScriptContext (s :: k)
  = PScriptContext (Term s (PDataList '[PTxInfo, PScriptPurpose]))
  deriving (PMatch, PIsData) via PIsDataReprInstances PScriptContext

instance PIsDataRepr PScriptContext where
  type PIsDataReprRepr PScriptContext =
      '[ '[ PTxInfo
          , PScriptPurpose
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PScriptContext) DRHNil

newtype PTxInfo (s :: k)
  = PTxInfo
      ( Term s
          ( PDataList
              '[ PBuiltinList PTxInInfo
               , PBuiltinList PTxOut
               , PValue
               , PValue
               , PBuiltinList PDCert
               , PMap PStakingCredential PInteger
               , PPOSIXTimeRange
               , PBuiltinList PPubKeyHash
               , PMap PScriptPurpose PRedeemer
               , PMap PDatumHash PDatum
               , PTxId
               ]
          )
      )
  deriving (PMatch, PIsData) via PIsDataReprInstances PTxInfo

instance PIsDataRepr PTxInfo where
  type
    PIsDataReprRepr PTxInfo =
      '[ '[ PBuiltinList PTxInInfo
          , PBuiltinList PTxOut
          , PValue
          , PValue
          , PBuiltinList PDCert
          , PMap PStakingCredential PInteger
          , PPOSIXTimeRange
          , PBuiltinList PPubKeyHash
          , PMap PScriptPurpose PRedeemer
          , PMap PDatumHash PDatum
          , PTxId
          ]
       ]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTxInfo) DRHNil













