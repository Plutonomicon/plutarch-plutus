{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Address (
  PCredential (PPubKeyCredential, PScriptCredential),
  PStakingCredential (PStakingHash, PStakingPtr),
  PAddress (PAddress),
) where

import PlutusLedgerApi.V1 qualified as Plutus

import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PScriptHash]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow, PTryFrom PData)
instance DerivePlutusType PCredential where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PCredential where type PLifted PCredential = Plutus.Credential
deriving via (DerivePConstantViaData Plutus.Credential PCredential) instance PConstantDecl Plutus.Credential
instance PTryFrom PData (PAsData PCredential)

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow, PTryFrom PData)
instance DerivePlutusType PStakingCredential where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PStakingCredential where type PLifted PStakingCredential = Plutus.StakingCredential
deriving via (DerivePConstantViaData Plutus.StakingCredential PStakingCredential) instance PConstantDecl Plutus.StakingCredential
instance PTryFrom PData (PAsData PStakingCredential)

newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataRecord
              '[ "credential" ':= PCredential
               , "stakingCredential" ':= PMaybeData PStakingCredential
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow, PTryFrom PData)
instance DerivePlutusType PAddress where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PAddress where type PLifted PAddress = Plutus.Address
deriving via (DerivePConstantViaData Plutus.Address PAddress) instance PConstantDecl Plutus.Address
instance PTryFrom PData (PAsData PAddress)
