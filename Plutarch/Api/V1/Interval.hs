{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Interval (
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PUpperBound (PUpperBound),
  PExtended (PFinite, PPosInf, PNegInf),
  type PClosure,
) where

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Prelude
import PlutusLedgerApi.V1.Interval qualified as Plutus

import Plutarch.Lift (
  PConstantDecl (PConstanted),
  PLifted,
  PUnsafeLiftDecl,
 )

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType (PInterval a) where type DPTStrat _ = PlutusTypeData

instance
  PLiftData a =>
  PUnsafeLiftDecl (PInterval a)
  where
  type PLifted (PInterval a) = (Plutus.Interval (PLifted a))
deriving via
  (DerivePConstantViaData (Plutus.Interval a) (PInterval (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.Interval a)

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType (PLowerBound a) where type DPTStrat _ = PlutusTypeData

instance
  PLiftData a =>
  PUnsafeLiftDecl (PLowerBound a)
  where
  type PLifted (PLowerBound a) = (Plutus.LowerBound (PLifted a))
deriving via
  (DerivePConstantViaData (Plutus.LowerBound a) (PLowerBound (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.LowerBound a)

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType (PUpperBound a) where type DPTStrat _ = PlutusTypeData

data PExtended a (s :: S)
  = PNegInf (Term s (PDataRecord '[]))
  | PFinite (Term s (PDataRecord '["_0" ':= a]))
  | PPosInf (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType (PExtended a) where type DPTStrat _ = PlutusTypeData

instance
  PLiftData a =>
  PUnsafeLiftDecl (PUpperBound a)
  where
  type PLifted (PUpperBound a) = (Plutus.UpperBound (PLifted a))
deriving via
  (DerivePConstantViaData (Plutus.UpperBound a) (PUpperBound (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.UpperBound a)
