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

  -- * Fields
  F.PDataFields (ptoFields, type PFields),
  F.pletFields,
  F.pletNFields,
  F.pletDropFields,
  F.pletRangeFields,
  F.pfield,
  F.hrecField,
  F.HRec,
) where

import qualified Plutarch.DataRepr.Field as F
import qualified Plutarch.DataRepr.Internal as I
