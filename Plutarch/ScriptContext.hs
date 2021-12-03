module Plutarch.ScriptContext (ScriptPurpose(..)) where

import Plutarch.Prelude
import Data.Kind (Type)

type CurrencySymbol = PTOpaque
type TxOutRef = PTOpaque
type StakingCredential = PTOpaque
type DCert = PTOpaque

data ScriptPurpose (exp :: PType -> Type) where
  Minting :: exp CurrencySymbol -> ScriptPurpose exp
  Spending :: exp TxOutRef -> ScriptPurpose exp
  Rewarding :: exp StakingCredential -> ScriptPurpose exp
  Certifying :: exp DCert -> ScriptPurpose exp
