{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Bool (
  PBool (..),
) where

import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable,
  PLifted (PLifted),
  pconstant,
 )
import Plutarch.Internal.PlutusType (
  PlutusType (PInner, pcon', pmatch'),
 )
import Plutarch.Internal.Term (
  S,
  pdelay,
  pforce,
  punsafeBuiltin,
  (#),
 )
import PlutusCore qualified as PLC

-- | Plutus 'BuiltinBool'
data PBool (s :: S) = PTrue | PFalse
  deriving stock (Show)

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBool Bool)
  instance
    PLiftable PBool

instance PlutusType PBool where
  type PInner PBool = PBool
  {-# INLINEABLE pcon' #-}
  pcon' =
    pconstant . \case
      PTrue -> True
      PFalse -> False
  {-# INLINEABLE pmatch' #-}
  pmatch' b f =
    pforce . pforce $ punsafeBuiltin PLC.IfThenElse # b # pdelay (f PTrue) # pdelay (f PFalse)
