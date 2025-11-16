{-# OPTIONS_GHC -Wno-deprecations #-}

module Plutarch.DataRepr
  {-# DEPRECATED "Use the new mechanism instead" #-} (
  -- * DataRepr
  I.PDataSum (PDataSum),
  I.punDataSum,
  I.ptryIndexDataSum,
  I.DataReprHandlers (DRHNil, DRHCons),
  I.PDataRecord (PDCons, PDNil),
  I.pdcons,
  I.pdnil,
  I.PLabeledType ((:=)),
  I.pindexDataRecord,
  I.pdropDataRecord,
  I.PlutusTypeData,

  -- * Fields
  F.PDataFields (ptoFields, type PFields),
  F.pletFields,
  F.pfield,
  F.HRec,
  F.HRecOf,
  F.PMemberFields,
  F.PMemberField,
) where

import Plutarch.DataRepr.Internal qualified as I
import Plutarch.DataRepr.Internal.Field qualified as F
