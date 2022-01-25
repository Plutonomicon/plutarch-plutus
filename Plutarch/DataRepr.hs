module Plutarch.DataRepr (
  -- * DataRepr
  I.PDataSum,
  I.punDataSum,
  I.ptryIndexDataSum,
  I.DataReprHandlers (DRHNil, DRHCons),
  I.PDataRecord,
  I.PLabeledType ((:=)),
  I.PIsDataRepr (type PIsDataReprRepr, pmatchRepr),
  I.pmatchDataSum,
  I.PIsDataReprInstances (PIsDataReprInstances),
  I.pindexDataRecord,
  I.pdropDataRecord,
  I.DerivePConstantViaData (DerivePConstantViaData),
  I.pasDataSum,

  -- * Fields
  F.PDataFields (ptoFields, type PFields),
  F.pletAllFields,
  F.pletNFields,
  F.pletFields,
  F.pletDropFields,
  F.pletRangeFields,
  F.pfield,
  F.hrecField,
  F.HRec,
) where

import qualified Plutarch.DataRepr.Internal as I
import qualified Plutarch.DataRepr.Internal.Field as F
