{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.TryFrom (
  PTryFrom (..),
  HRecP (..),
) where

import Data.Proxy (Proxy (Proxy))

import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, type (+))

import Plutarch.Unsafe (punsafeCoerce, punsafeFrom)

import Plutarch.Bool (pif, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Unit (PUnit (PUnit))

import Plutarch.Internal.Other (
  DerivePNewtype,
  PInner,
  POpaque,
  PType,
  S,
  Term,
  pcon,
  pdelay,
  pforce,
  plam,
  plet,
  popaque,
  (#),
  type (:-->),
 )

import Plutarch.Trace (ptraceError)

import Plutarch.DataRepr.Internal.HList (HRec (HCons, HNil), Labeled (Labeled))

import Plutarch.DataRepr.Internal (
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  pdnil,
 )

import Plutarch.List (
  phead,
  pmap,
  ptail,
 )

import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PBuiltinMap,
  PBuiltinPair,
  PData,
  PIsData (pfromData),
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pchooseListBuiltin,
  pdata,
  pforgetData,
  pfstBuiltin,
  ppairDataBuiltin,
  psndBuiltin,
 )

import Plutarch.TermCont (TermCont (TermCont, runTermCont), tcont, unTermCont)

import Plutarch.DataRepr.Internal (PIsDataRepr (PIsDataReprRepr), PIsDataReprInstances)

import Plutarch.Reducible (Reducible (Reduce))

import Data.Functor.Const (Const)

import Data.Kind (Type)

----------------------- The class PTryFrom ----------------------------------------------

{- |
    This checks the data structure for validity.
    If you don't care about parts of the structure
    don't verify those parts, just put a `PData` at
    the places you don't care about.
    Be aware this might get really expensive, so only
    use it if you cannot establish trust otherwise
    (e.g. via only checking a part of your Data with
    PTryFrom)
    Laws:
      - the operation `ptryFrom` mustn't change the representation of the underlying data
      - the operation `ptryFrom` must always prove the integrity of the whole target type
        - example:
          `ptryFrom PData (PAsData (PBuiltinList PData))` must only succeed if the underlying
          representation is a `BuiltinList` containing any `PData`
        - all conversions are fallible, this happens if the representation doesn't match
          the expected type.
      - the operation `ptryFrom @a @b` proves equality between the less expressive `PType` `a` and
        the more expressive `PType` `b`, hence the first element of the resulting Tuple
        must always be wrapped in `PAsData` if the origin type was `PData` (see law 1)
      - the result type `b` must always be safe than the origin type `a`, i.e. it must carry
        more information
-}
class PTryFrom (b :: PType) (a :: PType) where
  type PTryFromExcess b a :: PType
  ptryFrom :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess b a s)) -> Term s r) -> Term s r

----------------------- Reducible and Flip ----------------------------------------------

instance Reducible (f x y) => Reducible (Flip f y x) where
  type Reduce (Flip f y x) = Reduce (f x y)

newtype Flip f a b = Flip (f b a)

----------------------- HRecP and friends -----------------------------------------------

type family HRecPApply (as :: [(Symbol, PType)]) (s :: S) :: [Type] where
  HRecPApply ('(name, ty) ': rest) s = Labeled name (Reduce (ty s)) ': HRecPApply rest s
  HRecPApply '[] s = '[]

newtype HRecP (as :: [(Symbol, PType)]) (s :: S) = HRecP (HRec (HRecPApply as s))

instance Reducible (HRecP as s) where type Reduce (HRecP as s) = HRec (HRecPApply as s)

----------------------- PData instances -------------------------------------------------

instance PTryFrom (PAsData PInteger) PData where
  type PTryFromExcess (PAsData PInteger) PData = Flip Term PInteger
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, ver)

instance PTryFrom (PAsData PByteString) PData where
  type PTryFromExcess (PAsData PByteString) PData = Flip Term PByteString
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, ver)

instance
  ( PTryFrom (PAsData a) PData
  , PTryFrom (PAsData b) PData
  ) =>
  PTryFrom (PAsData (PBuiltinMap a b)) PData
  where
  type PTryFromExcess (PAsData (PBuiltinMap a b)) PData = Flip Term (PBuiltinMap a b)
  ptryFrom opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- tcont $ ptryFrom @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- tcont $ ptryFrom @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, ver)

{- |
    This verifies a list to be indeed a list but doesn't recover the inner data
    use this instance instead of the one for `PData (PAsData (PBuiltinList (PAsData a)))`
    as this is O(1) instead of O(n)
-}

-- TODO: add the excess inner type list
instance PTryFrom (PAsData (PBuiltinList PData)) PData where
  type PTryFromExcess (PAsData (PBuiltinList PData)) PData = Flip Term (PBuiltinList PData)
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, ver)

{- |
    Recover a `PBuiltinList (PAsData a)`
-}
instance
  ( PTryFrom (PAsData a) PData
  , PIsData a
  ) =>
  PTryFrom (PAsData (PBuiltinList (PAsData a))) PData
  where
  type PTryFromExcess (PAsData (PBuiltinList (PAsData a))) PData = Flip Term (PBuiltinList (PAsData a))
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pasList # opq
        verify :: Term _ (PData :--> PAsData a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- tcont $ ptryFrom @(PAsData a) $ e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, ver)

{- |
    Recover a `PAsData (PBuiltinPair a b)`
-}
instance
  ( PTryFrom a PData
  , a ~ PAsData a'
  , PIsData a'
  , PTryFrom b PData
  , b ~ PAsData b'
  , PIsData b'
  ) =>
  PTryFrom (PAsData (PBuiltinPair a b)) PData
  where
  type PTryFromExcess (PAsData (PBuiltinPair a b)) PData = Flip Term (PBuiltinPair a b)
  ptryFrom opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> tcont (ptryFrom @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> tcont (ptryFrom @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, ver)

----------------------- PDataRecord instances -------------------------------------------

-- We could have a more advanced instance but it's not needed really.

newtype ExcessForField (a :: PType) (s :: S) = ExcessForField (Term s (PAsData a), Reduce (PTryFromExcess (PAsData a) PData s))

instance Reducible (PTryFromExcess (PAsData a) PData s) => Reducible (ExcessForField a s) where
  type Reduce (ExcessForField a s) = (Term s (PAsData a), Reduce (PTryFromExcess (PAsData a) PData s))

-- FIXME: Should we always succede? If we always succede, performance would increase a lot.
instance PTryFrom (PDataRecord '[]) (PBuiltinList PData) where
  type PTryFromExcess (PDataRecord '[]) (PBuiltinList PData) = HRecP '[]
  ptryFrom opq = runTermCont $ do
    _ :: Term _ PUnit <-
      tcont . plet . pforce $
        pchooseListBuiltin # opq # pdelay (pcon PUnit) # pdelay (ptraceError "list is longer than zero")
    pure (pdnil, HNil)

type family UnHRecP (x :: PType) :: [(Symbol, PType)] where
  UnHRecP (HRecP as) = as

instance
  ( PTryFrom (PAsData pty) PData
  , PTryFrom (PDataRecord as) (PBuiltinList PData)
  , PTryFromExcess (PDataRecord as) (PBuiltinList PData) ~ HRecP ase
  ) =>
  PTryFrom (PDataRecord ((name ':= pty) ': as)) (PBuiltinList PData)
  where
  type
    PTryFromExcess (PDataRecord ((name ':= pty) ': as)) (PBuiltinList PData) =
      HRecP
        ( '(name, ExcessForField pty)
            ': UnHRecP (PTryFromExcess (PDataRecord as) (PBuiltinList PData))
        )
  ptryFrom opq = runTermCont $ do
    h <- tcont $ plet $ phead # opq
    hv <- tcont $ ptryFrom @(PAsData pty) @PData h
    t <- tcont $ plet $ ptail # opq
    tv <- tcont $ ptryFrom @(PDataRecord as) @(PBuiltinList PData) t
    pure (punsafeCoerce opq, HCons (Labeled hv) (snd tv))

newtype Helper a b s = Helper (a s, b s)

instance (Reducible (a s), Reducible (b s)) => Reducible (Helper a b s) where
  type Reduce (Helper a b s) = (Reduce (a s), Reduce (b s))

instance
  ( PTryFrom (PDataRecord as) (PBuiltinList PData)
  , PTryFromExcess (PDataRecord as) (PBuiltinList PData) ~ HRecP ase
  ) =>
  PTryFrom (PAsData (PDataRecord as)) PData
  where
  type
    PTryFromExcess (PAsData (PDataRecord as)) PData =
      Helper (Flip Term (PDataRecord as)) (PTryFromExcess (PDataRecord as) (PBuiltinList PData))
  ptryFrom opq = runTermCont $ do
    l <- snd <$> (tcont $ ptryFrom @(PAsData (PBuiltinList PData)) opq)
    r <- tcont $ ptryFrom @(PDataRecord as) l
    pure (punsafeCoerce opq, r)

instance {-# OVERLAPPING #-} SumValidation 0 ys => PTryFrom (PAsData (PDataSum ys)) PData where
  type PTryFromExcess (PAsData (PDataSum ys)) PData = Const ()
  ptryFrom opq = runTermCont $ do
    x <- tcont $ plet $ pasConstr # opq
    constr <- tcont $ plet $ pfstBuiltin # x
    fields <- tcont $ plet $ psndBuiltin # x
    _ <- tcont $ plet $ validateSum @0 @ys constr fields
    pure (punsafeCoerce opq, ())

class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) where
  validateSum :: Term s PInteger -> Term s (PBuiltinList PData) -> Term s POpaque

instance
  {-# OVERLAPPABLE #-}
  forall (n :: Nat) (x :: [PLabeledType]) (xs :: [[PLabeledType]]).
  ( PTryFrom (PDataRecord x) (PBuiltinList PData)
  , SumValidation (n + 1) xs
  , KnownNat n
  ) =>
  SumValidation n (x ': xs)
  where
  validateSum constr fields =
    pif
      (fromInteger (natVal $ Proxy @n) #== constr)
      ( unTermCont $ do
          _ <- tcont $ ptryFrom @(PDataRecord x) fields
          pure $ popaque $ pcon PUnit
      )
      (validateSum @(n + 1) @xs constr fields)

instance {-# OVERLAPPING #-} SumValidation n '[] where
  validateSum _ _ = ptraceError "reached end of sum while still not having found the constructor"

----------------------- other utility functions -----------------------------------------

{- | if there is an instance to recover something that is unwrapped from something that is
 unwrapped, then there is also the possibility to recover the whole thing but wrapped
-}
instance
  ( PTryFrom b a
  , PIsData a
  , PIsData b
  ) =>
  PTryFrom (PAsData b) (PAsData a)
  where
  type PTryFromExcess (PAsData b) (PAsData a) = PTryFromExcess b a
  ptryFrom opq = runTermCont $ do
    ver' <- snd <$> TermCont (ptryFrom @b @a (pfromData opq))
    pure $ (punsafeCoerce opq, ver')

instance PTryFrom PData PData where
  type PTryFromExcess PData PData = Const ()
  ptryFrom opq = runTermCont $ pure $ (opq, ())

instance PTryFrom PData (PAsData PData) where
  type PTryFromExcess PData (PAsData PData) = Const ()
  ptryFrom opq = runTermCont $ pure (pfromData opq, ())

instance PTryFrom (PAsData PData) PData where
  type PTryFromExcess (PAsData PData) PData = Const ()
  ptryFrom opq = runTermCont $ pure (pdata opq, ())

----------------------- PIsDataReprInstances instance -----------------------------------

-- | you can instantiate `PTryFrom` for your own datatype as demonstrated in the spec
instance
  ( PIsDataRepr a
  , SumValidation 0 (PIsDataReprRepr a)
  , PInner a b ~ PDataSum (PIsDataReprRepr a)
  ) =>
  PTryFrom (PAsData (PIsDataReprInstances a)) PData
  where
  type PTryFromExcess (PAsData (PIsDataReprInstances a)) PData = Const ()
  ptryFrom opq = runTermCont $ do
    let reprsum :: Term _ (PDataSum (PIsDataReprRepr a))
        reprsum = pfromData $ unTermCont $ fst <$> TermCont (ptryFrom opq)
    pure $ (pdata $ punsafeFrom reprsum, ())

----------------------- DerivePNewtype insatace -----------------------------------------

instance
  ( PTryFrom b a
  ) =>
  PTryFrom (DerivePNewtype c b) a
  where
  type PTryFromExcess (DerivePNewtype c b) a = PTryFromExcess b a
  ptryFrom opq = runTermCont $ (\(inn, exc) -> (punsafeFrom inn, exc)) <$> tcont (ptryFrom @b @a opq)

----------------------- HasField instance -----------------------------------------------

{-
instance (ElemOf name ptyp rec, Reduce (ptyp s) ~ out) => HasField name (HRecP rec s) out where
  getField = getExcessField @name
-}
