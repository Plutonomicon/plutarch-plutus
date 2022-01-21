{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Field (
  -- * PDataField class & deriving utils
  PDataFields (..),
  DerivePDataFields (..),
  pletFields,
  pletNFields,
  pletDropFields,
  pletRangeFields,
  pfield,
  pfield',

  -- * BindFields class mechanism
  BindFields (..),
  type TermsOf,
  type Take,
  type Drop,

  -- * Re-exports
  HList (..),
  HRec (..),
  hlistField,
  hrecField,
) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat)

import Plutarch.Builtin (
  PAsData,
  PData,
  PIsData (..),
  pasConstr,
  psndBuiltin,
 )
import Plutarch.DataRepr (
  PDataRecord,
  PIsDataRepr (..),
  PLabeled (..),
  pdhead,
  pdropDataRecord,
  pdtail,
  pindexDataRecord,
  pindexDataRecord',
  type PNames,
  type PTypes,
 )
import Plutarch.Field.HList (
  HList (..),
  HRec (..),
  hlistField,
  hrecField,
  type Drop,
  type IndexList,
  type IndexOf,
  type Range,
  type SingleItem,
  type Take,
 )
import Plutarch.Internal (TermCont (..), punsafeCoerce)
import Plutarch.Prelude

--------------------------------------------------------------------------------
---------- PDataField class & deriving utils

{- |
  Class allowing 'letFields' to work for a PType, usually via
  `PIsDataRepr`, but is derived for some other types for convenience.
-}
class PDataFields (a :: PType) where
  -- | Fields in HRec bound by 'letFields'
  type PFields a :: [PLabeled]

  -- | Convert a Term to a 'PDataList'
  ptoFields :: Term s a -> Term s (PDataRecord (PFields a))

{- |
  Derive PDataFields via a 'PIsDataRepr' instance,
  using either 'Numbered' fields or a given list of fields.
-}
newtype DerivePDataFields (p :: PType) (s :: S) = DerivePDataFields (p s)

instance PDataFields (PDataRecord as) where
  type PFields (PDataRecord as) = as
  ptoFields = id

instance
  forall a as.
  ( PIsDataRepr a
  , SingleItem (PIsDataReprRepr a) ~ as
  ) =>
  PDataFields (DerivePDataFields a)
  where
  type
    PFields (DerivePDataFields a) =
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
  (HRec (PNames (PFields a)) (TermsOf s (PTypes (PFields a))) -> Term s b) ->
  Term s b
pletFields t =
  runTermCont $
    fmap (HRec @(PNames (PFields a))) $ bindFields $ ptoFields t

{- | Bind a HRec of the first N fields.

  Always more efficient than binding all fields.
-}
pletNFields ::
  forall n a b s fs ns as.
  ( PDataFields a
  , fs ~ (Take n (PFields a))
  , ns ~ (PNames fs)
  , as ~ (PTypes fs)
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
  , ns ~ (PNames fs)
  , as ~ (PTypes fs)
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
  , ns ~ (PNames fs)
  , as ~ (PTypes fs)
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

-- | Map a list of 'PTypes' to the Terms that will be bound by 'bindFields'
type family TermsOf (s :: S) (as :: [PType]) :: [Type] where
  TermsOf _ '[] = '[]
  TermsOf s (x ': xs) = Term s (PAsData x) ': TermsOf s xs

class BindFields (as :: [PLabeled]) where
  -- |
  --    Bind all the fields in a 'PDataList' term to a corresponding
  --    HList of Terms.
  --
  --    A continuation is returned to enable sharing of
  --    the generated bound-variables.
  bindFields :: Term s (PDataRecord as) -> TermCont s (HList (TermsOf s (PTypes as)))

instance {-# OVERLAPS #-} BindFields ((l ':= a) ': '[]) where
  bindFields t =
    pure $ HCons (pdhead # t) HNil

instance {-# OVERLAPPABLE #-} (BindFields as) => BindFields ((l ':= a) ': as) where
  bindFields t = do
    t' <- TermCont $ plet t
    -- tail <- TermCont $ plet $ pdtail # t
    xs <- bindFields @as (pdtail # t')
    pure $ HCons (pdhead # t') xs

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
  , as ~ (PTypes (PFields p))
  , fs ~ (PNames (PFields p))
  , n ~ (IndexOf f fs)
  , KnownNat n
  , -- , KnownSymbol f
    a ~ (IndexList n as)
  ) =>
  Term s (p :--> PAsData a)
pfield =
  plam $ \t ->
    pindexDataRecord (Proxy @n) # ptoFields t

{- |
  Version of 'pfield' using repeated application of 'ptail', which may be
  more efficient in some cases.
-}
pfield' ::
  forall f p fs a as n s.
  ( PDataFields p
  , as ~ (PTypes (PFields p))
  , fs ~ (PNames (PFields p))
  , n ~ (IndexOf f fs)
  , KnownNat n
  , -- , KnownSymbol f
    a ~ (IndexList n as)
  ) =>
  Term s p ->
  Term s (PAsData a)
pfield' t =
  pindexDataRecord' (Proxy @n) (ptoFields t)
