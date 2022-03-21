{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.TryFrom (
  PTryFrom (..),
  HRecP (..),
  Flip (..),
  ptryFromData,
) where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))

import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, type (+))

import Plutarch.Unsafe (punsafeCoerce, punsafeFrom)

import Plutarch.Bool (pif, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Unit (PUnit (PUnit))

import Plutarch.Internal.Other (
  DerivePNewtype,
  PInner,
  PType,
  S,
  Term,
  pcon,
  pdelay,
  pforce,
  plam,
  plet,
  (#),
  type (:-->),
 )

import Plutarch.Trace (ptraceError)

import Plutarch.DataRepr.Internal (
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  pdcons,
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
class PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  ptryFrom :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r

-- | Utility function that saves a type application, `ptryFromData @b === ptryFrom @PData @b`
ptryFromData ::
  forall b s r.
  ( PTryFrom PData b
  ) =>
  Term s PData ->
  ((Term s b, Reduce (PTryFromExcess PData b s)) -> Term s r) ->
  Term s r
ptryFromData = ptryFrom @PData @b

----------------------- Reducible and Flip ----------------------------------------------

instance Reducible (HRecP as s) where type Reduce (HRecP as s) = HRecP as s

instance Reducible (f x y) => Reducible (Flip f y x) where
  type Reduce (Flip f y x) = Reduce (f x y)

newtype Flip f a b = MkFlip {unFlip :: f b a}

----------------------- HRecP and friends -----------------------------------------------

-- | Like @HRec@ but parametrised over a list of `PType`s, not `Type`s.
data HRecP (as :: [(Symbol, PType)]) (s :: S) where
  HNil :: HRecP '[] s
  HCons :: forall name a as s. Reduce (a s) -> HRecP as s -> HRecP ('(name, a) ': as) s

{- | allows accessing excess fields in an HRec, as of ghc921 you can
 also use `OverloadedRecordDot` with this.
-}
getExcessField ::
  forall name a as s.
  ( ElemOf name a as
  ) =>
  HRecP as s ->
  Reduce (a s)
getExcessField xs = indexHRec xs $ elemOf @name @a @as

-- | Index HRec using Elem
indexHRec :: forall s as. HRecP as s -> (forall a name. Elem name a as -> Reduce (a s))
indexHRec (HCons x _) Here = x
indexHRec (HCons _ xs) (There i) = indexHRec xs i
indexHRec HNil impossible = case impossible of {}

data Elem (sym :: Symbol) (a :: PType) (as :: [(Symbol, PType)]) where
  Here :: Elem sym a ('(sym, a) ': as)
  There :: Elem a sym as -> Elem a sym ('(sym', b) ': as)

class
  ElemOf (name :: Symbol) (a :: PType) (as :: [(Symbol, PType)])
    | as name -> a
  where
  elemOf :: Elem name a as

instance {-# OVERLAPPING #-} ElemOf name a ('(name, a) ': as) where
  elemOf = Here

instance
  {-# OVERLAPPABLE #-}
  ( ElemOf name a as
  ) =>
  ElemOf name a ('(name', b) ': as)
  where
  elemOf :: Elem name a ('(name', b) ': as)
  elemOf = There (elemOf @name @a @as)

----------------------- PData instances -------------------------------------------------

-- | recover a PInteger
instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = Flip Term PInteger
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, ver)

-- | recover a PByteString
instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = Flip Term PByteString
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, ver)

{- |
    recover a `PBuiltinMap`, the excess type contains the unwrapped map
    as well as a list containing the excess of all contained tuples
-}
instance
  ( PTryFrom PData (PAsData a)
  , PTryFrom PData (PAsData b)
  ) =>
  PTryFrom PData (PAsData (PBuiltinMap a b))
  where
  type PTryFromExcess PData (PAsData (PBuiltinMap a b)) = Flip Term (PBuiltinMap a b)
  ptryFrom opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- tcont $ ptryFromData @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- tcont $ ptryFromData @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, ver)

{- |
    This verifies a list to be indeed a list but doesn't recover the inner data
    use this instance instead of the one for `PData (PAsData (PBuiltinList (PAsData PData)))`
    as it is significantly faster
-}
instance PTryFrom PData (PAsData (PBuiltinList PData)) where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = Flip Term (PBuiltinList PData)
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, ver)

{- |
    Recover a `PBuiltinList (PAsData a)`
-}
instance
  ( PTryFrom PData (PAsData a)
  , PIsData a
  ) =>
  PTryFrom PData (PAsData (PBuiltinList (PAsData a)))
  where
  type PTryFromExcess PData (PAsData (PBuiltinList (PAsData a))) = Flip Term (PBuiltinList (PAsData a))
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pasList # opq
        verify :: Term _ (PData :--> PAsData a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- tcont $ ptryFromData @(PAsData a) $ e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, ver)

{- |
    Recover a `PAsData (PBuiltinPair a b)`
-}
instance
  ( PTryFrom PData a
  , a ~ PAsData a'
  , PIsData a'
  , PTryFrom PData b
  , b ~ PAsData b'
  , PIsData b'
  ) =>
  PTryFrom PData (PAsData (PBuiltinPair a b))
  where
  type PTryFromExcess PData (PAsData (PBuiltinPair a b)) = Flip Term (PBuiltinPair a b)
  ptryFrom opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> tcont (ptryFromData @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> tcont (ptryFromData @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, ver)

----------------------- PDataRecord instances -------------------------------------------

type FromRecordFields :: [PLabeledType] -> [(Symbol, PType)]
type family FromRecordFields xs where
  FromRecordFields '[] = '[]
  FromRecordFields ((label ':= ptyp) ': xs) = '(label, Flip Term ptyp) ': (FromRecordFields xs)

-- | Try to transform a `PBuiltinList PData` to a `PDataRecord`
instance PTryFrom (PBuiltinList PData) (PDataRecord '[]) where
  type PTryFromExcess (PBuiltinList PData) (PDataRecord '[]) = HRecP '[]
  ptryFrom opq = runTermCont $ do
    _ :: Term _ PUnit <-
      pchooseListBuiltin # opq # pdelay (pcon PUnit) # pdelay (ptraceError "list is longer than zero")
        & pforce
        & plet
        & tcont
    pure (pdnil, HNil)

instance
  ( PTryFrom (PBuiltinList PData) (PDataRecord xs)
  , PTryFrom PData (PAsData a)
  , PTryFromExcess (PBuiltinList PData) (PDataRecord xs) ~ HRecP (FromRecordFields xs)
  ) =>
  PTryFrom (PBuiltinList PData) (PDataRecord ((label ':= a) ': xs))
  where
  type
    PTryFromExcess (PBuiltinList PData) (PDataRecord ((label ':= a) ': xs)) =
      HRecP ('(label, PTryFromExcess PData (PAsData a)) ': FromRecordFields xs)
  ptryFrom lst = runTermCont $ do
    (verhead, exchead) <- tcont $ ptryFromData @(PAsData a) $ phead # lst
    (vertail, exctail) <- tcont $ ptryFrom @(PBuiltinList PData) @(PDataRecord xs) $ ptail # lst
    rec <- tcont $ plet $ pdcons @label # verhead # vertail
    pure (rec, HCons exchead exctail)

{- | recover a record from `PData`, the unwrapped record can be found in a field
 called "unwrapped"
-}
instance
  ( PTryFromExcess (PBuiltinList PData) (PDataRecord xs) ~ HRecP y
  , PTryFrom (PBuiltinList PData) (PDataRecord xs)
  ) =>
  PTryFrom PData (PAsData (PDataRecord xs))
  where
  type
    PTryFromExcess PData (PAsData (PDataRecord xs)) =
      HRecP '[ '("unwrapped", (Flip Term (PDataRecord xs)))] `HConcat` PTryFromExcess (PBuiltinList PData) (PDataRecord xs)
  ptryFrom opq = runTermCont $ do
    lst <- snd <$> tcont (ptryFromData @(PAsData (PBuiltinList PData)) opq)
    (rec, exc) <- tcont $ (ptryFrom @(PBuiltinList PData) @(PDataRecord xs) lst)
    pure (punsafeCoerce opq, HCons rec exc)

type family rec1 `HConcat` rec2 where
  HRecP '[] `HConcat` HRecP l1 = HRecP l1
  HRecP (e ': l0) `HConcat` HRecP l1 = HRecP l0 `HConcat` HRecP (e ': l1)

----------------------- PDataSum Instances ----------------------------------------------

{- | recover a PDataSum, this is really slow as it requires the whole record to be recovered
 if possible try to first recover a `PDataSum` containing only `PAsData PData`and then recover
 the field itself after matching on the constructor
-}
instance
  forall ys.
  ( SumValidation 0 ys
  ) =>
  PTryFrom PData (PAsData (PDataSum ys))
  where
  type PTryFromExcess PData (PAsData (PDataSum ys)) = HRecP '[]
  ptryFrom opq = runTermCont $ do
    _ <- tcont $ plet $ validateSum @0 @ys opq
    pure (punsafeCoerce opq, HNil)

class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) where
  validateSum :: Term s PData -> Term s (PBuiltinList PData)

instance
  {-# OVERLAPPABLE #-}
  forall (n :: Nat) (x :: [PLabeledType]) (xs :: [[PLabeledType]]).
  ( PTryFrom PData (PAsData (PDataRecord x))
  , SumValidation (n + 1) xs
  , KnownNat n
  , PTryFrom (PBuiltinList PData) (PDataRecord x)
  ) =>
  SumValidation n (x ': xs)
  where
  validateSum s = unTermCont $
    do
      let n :: Integer
          n = natVal (Proxy @n)
      elem <- tcont $ plet $ pasConstr # s
      let snd' :: Term _ (PBuiltinList PData)
          snd' =
            pif
              (fromInteger n #== (pfstBuiltin # elem))
              ( unTermCont $ do
                  y <- fst <$> tcont (ptryFrom @(PBuiltinList PData) @(PDataRecord x) (psndBuiltin # elem))
                  pure $ punsafeCoerce (y :: Term _ (PDataRecord x))
              )
              (validateSum @(n + 1) @xs $ punsafeCoerce s)
      tcont $ plet snd'

instance {-# OVERLAPPING #-} SumValidation n '[] where
  validateSum _ = ptraceError "reached end of sum while still not having found the constructor"

----------------------- other utility functions -----------------------------------------

{- | if there is an instance to recover something that is unwrapped from something that is
 unwrapped, then there is also the possibility to recover the whole thing but wrapped
-}
instance
  ( PTryFrom a b
  , PIsData a
  , PIsData b
  ) =>
  PTryFrom (PAsData a) (PAsData b)
  where
  type PTryFromExcess (PAsData a) (PAsData b) = PTryFromExcess a b
  ptryFrom opq = runTermCont $ do
    ver' <- snd <$> TermCont (ptryFrom @a @b (pfromData opq))
    pure $ (punsafeCoerce opq, ver')

-- | serves as the base case for some conversions
instance PTryFrom PData PData where
  type PTryFromExcess PData PData = HRecP '[]
  ptryFrom opq = runTermCont $ pure $ (opq, HNil)

-- | serves as the base case for some conversions
instance PTryFrom PData (PAsData PData) where
  type PTryFromExcess PData (PAsData PData) = Flip Term PData
  ptryFrom opq = runTermCont $ pure (pdata opq, opq)

----------------------- PIsDataReprInstances instance -----------------------------------

-- | you can instantiate `PTryFrom` for your own datatype as demonstrated in the spec
instance
  ( PIsDataRepr a
  , SumValidation 0 (PIsDataReprRepr a)
  , PInner a b ~ PDataSum (PIsDataReprRepr a)
  ) =>
  PTryFrom PData (PAsData (PIsDataReprInstances a))
  where
  type PTryFromExcess PData (PAsData (PIsDataReprInstances a)) = HRecP '[]
  ptryFrom opq = runTermCont $ do
    let reprsum :: Term _ (PDataSum (PIsDataReprRepr a))
        reprsum = pfromData $ unTermCont $ fst <$> TermCont (ptryFrom opq)
    pure $ (pdata $ punsafeFrom reprsum, HNil)

----------------------- DerivePNewtype insatace -----------------------------------------

instance
  ( PTryFrom a b
  ) =>
  PTryFrom a (DerivePNewtype c b)
  where
  type PTryFromExcess a (DerivePNewtype c b) = PTryFromExcess a b
  ptryFrom opq = runTermCont $ (\(inn, exc) -> (punsafeFrom inn, exc)) <$> tcont (ptryFrom @a @b opq)

----------------------- HasField instance -----------------------------------------------

instance (ElemOf name ptyp rec, Reduce (ptyp s) ~ out) => HasField name (HRecP rec s) out where
  getField = getExcessField @name
