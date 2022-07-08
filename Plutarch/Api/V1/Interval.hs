{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Api.V1.Interval (
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PUpperBound (PUpperBound),
  PExtended (PFinite, PPosInf, PNegInf),
  type PClosure,
) where

import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, POrd)
instance DerivePlutusType (PInterval a) where type DPTStrat _ = PlutusTypeData

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, POrd)
instance DerivePlutusType (PLowerBound a) where type DPTStrat _ = PlutusTypeData

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, POrd)
instance DerivePlutusType (PUpperBound a) where type DPTStrat _ = PlutusTypeData

data PExtended a (s :: S)
  = PNegInf (Term s (PDataRecord '[]))
  | PFinite (Term s (PDataRecord '["_0" ':= a]))
  | PPosInf (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType (PExtended a) where type DPTStrat _ = PlutusTypeData
