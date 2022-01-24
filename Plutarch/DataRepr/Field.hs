{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Field (
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
  hrecField,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat)

import Plutarch.Builtin (
  PAsData,
  PData,
  PIsData (pfromData),
  pasConstr,
  psndBuiltin,
 )
import Plutarch.DataRepr.Internal (
  PDataRecord,
  PIsDataRepr (type PIsDataReprRepr),
  PIsDataReprInstances,
  PLabeledType ((:=)),
  pdropDataRecord,
  pindexDataRecord,
  type PLabel,
  type PUnLabel,
 )
import Plutarch.DataRepr.Internal.HList (
  HList (HCons, HNil),
  HRec (HRec),
  hrecField,
  type Drop,
  type IndexList,
  type IndexOf,
  type Range,
  type SingleItem,
  type Take,
 )
import Plutarch.Internal (TermCont (TermCont, runTermCont), punsafeCoerce)
import Plutarch.Prelude

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

instance
  forall a.
  ( PIsDataRepr a
  ) =>
  PDataFields (PIsDataReprInstances a)
  where
  type
    PFields (PIsDataReprInstances a) =
      SingleItem (PIsDataReprRepr a)

  ptoFields t =
    (punsafeCoerce $ phoistAcyclic $ plam $ \d -> psndBuiltin #$ pasConstr # d)
      # (punsafeCoerce t :: Term _ PData)

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
  forall a b s.
  ( PDataFields a
  , BindFields (PFields a)
  ) =>
  Term s a ->
  (HRec (PLabel (PFields a)) (TermsOf s (PUnLabel (PFields a))) -> Term s b) ->
  Term s b
pletFields t =
  runTermCont $
    fmap (HRec @(PLabel (PFields a))) $ bindFields $ ptoFields t

{- | Bind a HRec of the first N fields.

  Always more efficient than binding all fields.
-}
pletNFields ::
  forall n a b s fs ns as.
  ( PDataFields a
  , fs ~ (Take n (PFields a))
  , ns ~ (PLabel fs)
  , as ~ (PUnLabel fs)
  , BindFields fs
  ) =>
  Term s a ->
  ((HRec ns) (TermsOf s as) -> Term s b) ->
  Term s b
pletNFields t =
  runTermCont $
    fmap (HRec @ns) $ bindFields $ to $ ptoFields t
  where
    to :: Term s (PDataRecord (PFields a)) -> Term s (PDataRecord fs)
    to = punsafeCoerce

{- | Bind a HRec, dropping the first N fields.

  Usually more efficient than binding all fields.
-}
pletDropFields ::
  forall n a b s fs ns as.
  ( PDataFields a
  , fs ~ (Drop n (PFields a))
  , ns ~ (PLabel fs)
  , as ~ (PUnLabel fs)
  , BindFields fs
  , KnownNat n
  ) =>
  Term s a ->
  ((HRec ns) (TermsOf s as) -> Term s b) ->
  Term s b
pletDropFields t =
  runTermCont $
    fmap (HRec @ns) $ bindFields $ to $ ptoFields t
  where
    to :: Term s (PDataRecord (PFields a)) -> Term s (PDataRecord fs)
    to = pdropDataRecord (Proxy @n)

{- | Bind a HRec,

  Usually more efficient than binding all fields.
-}
pletRangeFields ::
  forall to from a b s fs ns as.
  ( PDataFields a
  , fs ~ (Range to from (PFields a))
  , ns ~ (PLabel fs)
  , as ~ (PUnLabel fs)
  , BindFields fs
  , KnownNat to
  , KnownNat from
  ) =>
  Term s a ->
  ((HRec ns) (TermsOf s as) -> Term s b) ->
  Term s b
pletRangeFields t =
  runTermCont $
    fmap (HRec @ns) $ bindFields $ to $ ptoFields t
  where
    to :: Term s (PDataRecord (PFields a)) -> Term s (PDataRecord fs)
    to r = punsafeCoerce $ pdropDataRecord (Proxy @from) r

-- | Map a list of 'PUnLabel' to the Terms that will be bound by 'bindFields'
type family TermsOf (s :: S) (as :: [PType]) :: [Type] where
  TermsOf _ '[] = '[]
  TermsOf s (x ': xs) = Term s (PAsData x) ': TermsOf s xs

class BindFields (as :: [PLabeledType]) where
  -- |
  --    Bind all the fields in a 'PDataList' term to a corresponding
  --    HList of Terms.
  --
  --    A continuation is returned to enable sharing of
  --    the generated bound-variables.
  bindFields :: Term s (PDataRecord as) -> TermCont s (HList (TermsOf s (PUnLabel as)))

instance {-# OVERLAPPING #-} BindFields ((l ':= a) ': '[]) where
  bindFields t =
    pure $ HCons (pindexDataRecord (Proxy @0) t) HNil

instance {-# OVERLAPPABLE #-} (BindFields as) => BindFields ((l ':= a) ': as) where
  bindFields t = do
    t' <- TermCont $ plet t
    xs <- bindFields @as (pdropDataRecord (Proxy @1) t')
    pure $ HCons (pindexDataRecord (Proxy @0) t') xs

--
--------------------------------------------------------------------------------

{- |
  Get a single field from a Term.
  Use this where you only need a single field,
  as it may be  more efficient than the bindings generated by
  'letFields'
-}
pfield ::
  forall f p fs a as n s.
  ( PDataFields p
  , as ~ (PUnLabel (PFields p))
  , fs ~ (PLabel (PFields p))
  , n ~ (IndexOf f fs)
  , KnownNat n
  , a ~ (IndexList n as)
  ) =>
  Term s (p :--> PAsData a)
pfield =
  plam $ \t ->
    pindexDataRecord (Proxy @n) $ ptoFields t
