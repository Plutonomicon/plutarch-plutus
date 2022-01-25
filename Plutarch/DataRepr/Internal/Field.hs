{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.Field (
  -- * PDataField class & deriving utils
  PDataFields (..),
  pletFields,
  pletNFields,
  pletDropFields,
  pletRangeFields,
  pfield,

  -- * BindFields class mechanism
  BindFields (..),
  type TermsOf,
  type Take,
  type Drop,

  -- * Re-exports
  HRec (..),
  Labeled (Labeled, unLabeled),
  hrecField,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat)

import Data.Kind (Type)
import Plutarch (PType, S, Term, plam, plet, (#), (#$), type (:-->))
import Plutarch.Builtin (
  PAsData,
  PIsData (pfromData),
 )
import Plutarch.DataRepr.Internal (
  PDataRecord,
  PDataSum,
  PIsDataRepr (type PIsDataReprRepr),
  PIsDataReprInstances,
  PLabeledType ((:=)),
  pasDataSum,
  pdropDataRecord,
  pindexDataRecord,
  punDataSum,
  type PLabelIndex,
  type PUnLabel,
 )
import Plutarch.DataRepr.Internal.HList (
  HRec (HCons, HNil),
  Labeled (Labeled, unLabeled),
  hrecField,
  type Drop,
  type IndexList,
  type Range,
  type SingleItem,
  type Take,
 )
import Plutarch.Internal (TermCont (TermCont, runTermCont), punsafeCoerce)

--------------------------------------------------------------------------------
---------- PDataField class & deriving utils

{- |
  Class allowing 'letFields' to work for a PType, usually via
  `PIsDataRepr`, but is derived for some other types for convenience.
-}
class PDataFields (a :: PType) where
  -- | Fields in HRec bound by 'letFields'
  type PFields a :: [PLabeledType]

  -- | Convert a Term to a 'PDataList'
  ptoFields :: Term s a -> Term s (PDataRecord (PFields a))

instance PDataFields (PDataRecord as) where
  type PFields (PDataRecord as) = as
  ptoFields = id

instance PDataFields (PDataSum '[as]) where
  type PFields (PDataSum '[as]) = as
  ptoFields = (punDataSum #)

instance
  forall a fields.
  ( PIsDataRepr a
  , PIsDataReprRepr a ~ '[fields]
  , SingleItem (PIsDataReprRepr a) ~ fields
  ) =>
  PDataFields (PIsDataReprInstances a)
  where
  type
    PFields (PIsDataReprInstances a) =
      SingleItem (PIsDataReprRepr a)

  ptoFields x = punDataSum #$ pasDataSum (punsafeCoerce x :: Term _ a)

instance
  forall a.
  ( PIsData a
  , PDataFields a
  ) =>
  PDataFields (PAsData a)
  where
  type PFields (PAsData a) = PFields a
  ptoFields = ptoFields . pfromData

{- |
  Bind a HRec of named fields from a compatible type.
-}
pletFields ::
  forall a b as s.
  ( PDataFields a
  , as ~ (PFields a)
  , BindFields as
  ) =>
  Term s a ->
  (HRec (TermsOf s as) -> Term s b) ->
  Term s b
pletFields t =
  runTermCont $
    bindFields $ ptoFields t

{- | Bind a HRec of the first N fields.

  Always more efficient than binding all fields.
-}
pletNFields ::
  forall n a b s as.
  ( PDataFields a
  , as ~ (Take n (PFields a))
  , BindFields as
  ) =>
  Term s a ->
  (HRec (TermsOf s as) -> Term s b) ->
  Term s b
pletNFields t =
  runTermCont $
    bindFields $ to $ ptoFields t
  where
    to :: Term s (PDataRecord (PFields a)) -> Term s (PDataRecord as)
    to = punsafeCoerce

{- | Bind a HRec, dropping the first N fields.

  Usually more efficient than binding all fields.
-}
pletDropFields ::
  forall n a b s as.
  ( PDataFields a
  , as ~ (Drop n (PFields a))
  , BindFields as
  , KnownNat n
  ) =>
  Term s a ->
  (HRec (TermsOf s as) -> Term s b) ->
  Term s b
pletDropFields t =
  runTermCont $
    bindFields $ to $ ptoFields t
  where
    to :: Term s (PDataRecord (PFields a)) -> Term s (PDataRecord as)
    to = pdropDataRecord (Proxy @n)

{- | Bind a HRec,

  Usually more efficient than binding all fields.
-}
pletRangeFields ::
  forall to from a b s as.
  ( PDataFields a
  , as ~ (Range to from (PFields a))
  , BindFields as
  , KnownNat to
  , KnownNat from
  ) =>
  Term s a ->
  ((HRec (TermsOf s as)) -> Term s b) ->
  Term s b
pletRangeFields t =
  runTermCont $
    bindFields @as $ to $ ptoFields t
  where
    to :: Term s (PDataRecord (PFields a)) -> Term s (PDataRecord fs)
    to r = punsafeCoerce $ pdropDataRecord (Proxy @from) r

-- | Map a list of 'PUnLabel' to the Terms that will be bound by 'bindFields'
type family TermsOf (s :: S) (as :: [PLabeledType]) :: [Type] where
  TermsOf _ '[] = '[]
  TermsOf s ((name ':= a) ': as) = (Labeled name (Term s (PAsData a))) ': TermsOf s as

class BindFields (as :: [PLabeledType]) where
  -- |
  --    Bind all the fields in a 'PDataList' term to a corresponding
  --    HList of Terms.
  --
  --    A continuation is returned to enable sharing of
  --    the generated bound-variables.
  bindFields :: Term s (PDataRecord as) -> TermCont s (HRec (TermsOf s as))

instance {-# OVERLAPPING #-} BindFields ((l ':= a) ': '[]) where
  bindFields t =
    pure $ HCons (Labeled $ pindexDataRecord (Proxy @0) t) HNil

instance {-# OVERLAPPABLE #-} (BindFields as) => BindFields ((l ':= a) ': as) where
  bindFields t = do
    t' <- TermCont $ plet t
    xs <- bindFields @as (pdropDataRecord (Proxy @1) t')
    pure $ HCons (Labeled $ pindexDataRecord (Proxy @0) t') xs

--
--------------------------------------------------------------------------------

{- |
  Get a single field from a Term.
  Use this where you only need a single field,
  as it may be  more efficient than the bindings generated by
  'letFields'
-}
pfield ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ (PFields p)
  , n ~ (PLabelIndex name as)
  , KnownNat n
  , a ~ (PUnLabel (IndexList n as))
  ) =>
  Term s (p :--> PAsData a)
pfield =
  plam $ \t ->
    pindexDataRecord (Proxy @n) $ ptoFields @p t
