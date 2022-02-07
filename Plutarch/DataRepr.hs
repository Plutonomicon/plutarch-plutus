module Plutarch.DataRepr (
  -- * DataRepr
  I.PDataSum,
  I.punDataSum,
  I.ptryIndexDataSum,
  I.DataReprHandlers (DRHNil, DRHCons),
  I.PDataRecord,
  pdcons,
  pdnil,
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
  F.pletFields,
  F.pfield,
  F.hrecField,
  F.HRec,
) where

import Plutarch (Term, type (:-->))
import Plutarch.Builtin (PAsData, PBuiltinList, PData)
import qualified Plutarch.DataRepr.Internal as I
import qualified Plutarch.DataRepr.Internal.Field as F
import Plutarch.Internal (punsafeCoerce)
import Plutarch.List (pcons, pnil)

{- | Cons a field to a data record.

You can specify the label to associate with the field using type applications-

@

foo :: Term s (PDataRecord '[ "fooField" ':= PByteString ])
foo = pdcons @"fooField" # pdata (phexByteStr "ab") # pdnil

@
-}
pdcons :: forall label a l s. Term s (PAsData a :--> I.PDataRecord l :--> I.PDataRecord ((label 'I.:= a) ': l))
pdcons = punsafeCoerce $ pcons @PBuiltinList @(PAsData a)

-- | An empty 'PDataRecord'.
pdnil :: Term s (I.PDataRecord '[])
pdnil = punsafeCoerce $ pnil @PBuiltinList @PData
