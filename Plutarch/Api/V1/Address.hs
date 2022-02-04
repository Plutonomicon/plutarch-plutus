{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Address (
  PCredential (PPubKeyCredential, PScriptCredential),
  PStakingCredential (PStakingHash, PStakingPtr),
  PAddress (PAddress),
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V1.Scripts (PValidatorHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
  PLabeledType ((:=)),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PValidatorHash]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via (PIsDataReprInstances PCredential)

instance PUnsafeLiftDecl PCredential where type PLifted PCredential = Plutus.Credential
deriving via (DerivePConstantViaData Plutus.Credential PCredential) instance (PConstant Plutus.Credential)

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
    (PlutusType, PIsData)
    via PIsDataReprInstances PStakingCredential

instance PUnsafeLiftDecl PStakingCredential where type PLifted PStakingCredential = Plutus.StakingCredential
deriving via (DerivePConstantViaData Plutus.StakingCredential PStakingCredential) instance (PConstant Plutus.StakingCredential)

newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataRecord
              '[ "credential" ':= PCredential
               , "stakingCredential" ':= (PMaybeData PStakingCredential)
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PAddress

instance PUnsafeLiftDecl PAddress where type PLifted PAddress = Plutus.Address
deriving via (DerivePConstantViaData Plutus.Address PAddress) instance (PConstant Plutus.Address)
