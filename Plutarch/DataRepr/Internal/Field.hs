{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.Field (
  -- * PDataField class & deriving utils
  PDataFields (..),
  pletFields,
  pfield,

  -- * BindFields class mechanism
  BindFields (..),
  type BoundTerms,
  type Drop,

  -- * Re-exports
  HRec (..),
  Labeled (Labeled, unLabeled),
  hrecField,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (
  KnownNat,
  Symbol,
 )

import Data.Kind (Type)
import Plutarch (
  PType,
  S,
  Term,
  plam,
  plet,
  (#),
  (#$),
  type (:-->),
 )

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
import Plutarch.DataRepr.Internal.FromData (PFromDataable, pmaybeFromAsData)
import Plutarch.DataRepr.Internal.HList (
  HRec (HCons, HNil),
  Labeled (Labeled, unLabeled),
  hrecField,
  type Drop,
  type IndexList,
  type SingleItem,
 )
import Plutarch.Internal (punsafeCoerce)
import Plutarch.TermCont (TermCont (TermCont), runTermCont)

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
  Bind a HRec of named fields containing all the specified
  fields.
-}
pletFields ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ (PFields a)
  , bs ~ (Bindings ps fs)
  , BindFields ps bs
  ) =>
  Term s a ->
  (HRec (BoundTerms ps bs s) -> Term s b) ->
  Term s b
pletFields t =
  runTermCont $
    bindFields @ps @bs $ ptoFields @a t

data ToBind = Bind | Skip

{- | Get whether a field should be bound based on its inclusion in a
     list of fields.
-}
type family BindField (p :: Symbol) (fs :: [Symbol]) :: ToBind where
  BindField _ '[] = 'Skip
  BindField name (name ': _) = 'Bind
  BindField name (_ ': as) = BindField name as

-- | Map 'BindField' over @[PLabeledType]@, with 'Skips' removed at tail
type family Bindings (ps :: [PLabeledType]) (fs :: [Symbol]) :: [ToBind] where
  Bindings '[] _ = '[]
  Bindings ((name ':= _) ': ps) fs = (BindField name fs) ': (CutSkip (Bindings ps fs))

-- | Remove 'Skip's at tail
type family CutSkip (bs :: [ToBind]) :: [ToBind] where
  CutSkip '[ 'Skip] = '[]
  CutSkip bs = bs

{- |
  Get the 'Term' representations to be bound based on the
  result of 'Bindings'.
-}
type BoundTerms :: [PLabeledType] -> [ToBind] -> S -> [(Symbol, Type)]
type family BoundTerms ps bs s where
  BoundTerms '[] _ _ = '[]
  BoundTerms _ '[] _ = '[]
  BoundTerms (_ ': ps) ( 'Skip ': bs) s = BoundTerms ps bs s
  BoundTerms ((name ':= p) ': ps) ( 'Bind ': bs) s = '(name, Term s (PAsData p)) ': BoundTerms ps bs s

class BindFields (ps :: [PLabeledType]) (bs :: [ToBind]) where
  -- |
  --    Bind all the fields in a 'PDataList' term to a corresponding
  --    HList of Terms.
  --
  --    A continuation is returned to enable sharing of
  --    the generated bound-variables.
  bindFields :: Term s (PDataRecord ps) -> TermCont s (HRec (BoundTerms ps bs s))

instance {-# OVERLAPPING #-} BindFields ((l ':= p) ': ps) ( 'Bind ': '[]) where
  bindFields t =
    pure $ HCons (Labeled $ pindexDataRecord (Proxy @0) t) HNil

instance {-# OVERLAPPABLE #-} (BindFields ps bs) => BindFields ((l ':= p) ': ps) ( 'Bind ': bs) where
  bindFields t = do
    t' <- TermCont $ plet t
    xs <- bindFields @ps @bs (pdropDataRecord (Proxy @1) t')
    pure $ HCons (Labeled $ pindexDataRecord (Proxy @0) t') xs

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': ps) ( 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @1) t

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': p2 ': ps) ( 'Skip ': 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @2) t

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': ps) ( 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @3) t

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @4) t

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': p5 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @5) t

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': p5 ': p6 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @6) t

instance {-# OVERLAPPING #-} (BindFields ps bs) => BindFields (p1 ': p2 ': p3 ': p4 ': p5 ': p6 ': p7 ': ps) ( 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': 'Skip ': bs) where
  bindFields t = do
    bindFields @ps @bs $ pdropDataRecord (Proxy @7) t

--------------------------------------------------------------------------------

{- |
  Get a single field from a Term.

  *NB*: If you access more than one field from
  the same value you should use 'pletFields' instead,
  which will generate the bindings more efficiently.
-}
pfield ::
  forall name p s a as n b.
  ( PDataFields p
  , as ~ (PFields p)
  , n ~ (PLabelIndex name as)
  , KnownNat n
  , a ~ (PUnLabel (IndexList n as))
  , PFromDataable a b
  ) =>
  Term s (p :--> b)
pfield = plam $ \i ->
  pmaybeFromAsData $ pindexDataRecord (Proxy @n) $ ptoFields @p i
