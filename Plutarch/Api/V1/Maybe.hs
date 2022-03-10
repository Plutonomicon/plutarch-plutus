{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Api.V1.Maybe (
  PMaybeData (PDJust, PDNothing),
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances), PIsDataReprRepr)
import Plutarch.DataRepr.Internal.HList.Utils (IndexList)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

-- | Data encoded Maybe type. Used in various ledger api types.
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PEq)
    via PIsDataReprInstances (PMaybeData a)

-- Have to manually write this instance because the constructor id ordering is screwed for 'Maybe'....
instance (PIsData a, POrd a) => POrd (PMaybeData a) where
  x #< y = unTermCont $ do
    a <- tcont . plet $ pasConstr #$ pforgetData $ pdata x
    b <- tcont . plet $ pasConstr #$ pforgetData $ pdata y

    cid1 <- tcont . plet $ pfstBuiltin # a
    cid2 <- tcont . plet $ pfstBuiltin # b

    pure $
      pif
        (cid1 #< cid2)
        (pconstant False)
        $ pif
          (cid1 #== cid2)
          {- Some hand optimization here: usually, the fields would be 'plet'ed here if using 'POrd' derivation
            machinery. However, in this case - there's no need for the fields for the 'Nothing' case.

            Would be nice if this could be done on the auto derivation case....
          -}
          ( pif
              (cid1 #== 0)
              (lt0 (punsafeCoerce $ psndBuiltin # a) (punsafeCoerce $ psndBuiltin # b))
              -- Both are 'Nothing'. '#<' is False.
              $ pconstant False
          )
          $ pconstant True
    where
      lt0 = (#<) @(PDataRecord (IndexList 0 (PIsDataReprRepr (PMaybeData a))))

  x #<= y = x #== y #|| x #< y
