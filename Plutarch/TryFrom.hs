{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Plutarch.TryFrom (
  PTryFrom (..),
  PTryFromData (..),
  HRecP (..),
  RecKind (..),
  Flip (..),
  HSing,
  HSSing,
  hsing,
  getExcessField,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, type (+))

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
  pdata,
  pforgetData,
  pfstBuiltin,
  ppairDataBuiltin,
  psndBuiltin,
 )

import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal.Other (
  POpaque,
  PType,
  S,
  Term,
  PInner,
  perror,
  plam,
  plet,
  (#),
  type (:-->),
 )

import Plutarch.DataRepr.Internal (
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  pdcons,
  pdnil,
 )

import Plutarch.Bool (pif, (#==))

import Plutarch.List (
  phead,
  pmap,
  ptail,
 )

import Plutarch.Unsafe (punsafeCoerce, punsafeFrom)

import GHC.Records (HasField (getField))

import Plutarch.TermCont (TermCont (TermCont, runTermCont), tcont, unTermCont)

import Plutarch.DataRepr.Internal (PIsDataReprInstances, PIsDataRepr (PIsDataReprRepr))

----------------------- The class PTryFrom ----------------------------------------------

{- |
    This checks the datastructure for validity.
    If you don't care about parts of the datastructure
    don't verify those parts, just let it return a PData
    instead
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
        - all conversion are fallible, this happens if the representation doesn't match
          the expected type.
      - the operation `ptryFrom` proves equality between the less expressive `PType` `a` and
        the more expressive `PType` `b`, hence the first element of the resulting Tuple
        must always be wrapped in `PAsData` if the origin type was `PData` (see law 1)
      - the result type `b` must always be safe than the origin type `a`, i.e. it must carry
        more information
-}
class PTryFrom (a :: PType) (b :: PType) (s :: S) where
  type PTryFromExcess a b :: PType
  ptryFrom :: Term s a -> ((Term s b, PTryFromExcess a b s) -> Term s r) -> Term s r

----------------------- HTree and friends -----------------------------------------------

newtype Flip f a b = MkFlip {unFlip :: f b a}

data RecKind
  = HNil
  | HCons Symbol PType RecKind

type HSing :: Symbol -> PType -> RecKind
type family HSing sym typ where
  HSing sym typ = 'HCons sym (Flip Term typ) 'HNil

data HRecP (as :: RecKind) (s :: S) where
  HSNil :: HRecP 'HNil s
  HSCons ::
    forall name a s as.
    a s ->
    HRecP as s ->
    HRecP ( 'HCons name a as) s

type HSSing :: Symbol -> PType -> PType
type HSSing sym a = HRecP (HSing sym a)

hsing :: forall sym a (s :: S). Term s a -> HSSing sym a s
hsing ter = HSCons @sym (MkFlip ter) HSNil

getExcessField ::
  forall name a b as s.
  ( ElemOf name a as
  , MaybeUnFlip a s b
  ) =>
  HRecP as s ->
  b
getExcessField xs = maybeUnFlip $ indexHRec xs $ elemOf @name @a @as

class MaybeUnFlip a s b | a s -> b where
  maybeUnFlip :: a s -> b

instance {-# OVERLAPPING #-} MaybeUnFlip (Flip Term a) s (Term s a) where
  maybeUnFlip = unFlip

instance {-# OVERLAPPABLE #-} MaybeUnFlip (HRecP reck) s ((HRecP reck) s) where
  maybeUnFlip = id

-- | Index HRec using Elem
indexHRec :: forall s as. HRecP as s -> (forall a name. Elem name a as -> a s)
indexHRec (HSCons x _) Here = x
indexHRec (HSCons _ xs) (There i) = indexHRec xs i
indexHRec HSNil impossible = case impossible of {}

data Elem (sym :: Symbol) (a :: PType) (as :: RecKind) where
  Here :: Elem sym a ( 'HCons sym a as)
  There :: Elem a sym as -> Elem a sym ( 'HCons sym' b as)

class
  ElemOf (name :: Symbol) (a :: PType) (as :: RecKind)
    | as name -> a
  where
  elemOf :: Elem name a as

instance {-# OVERLAPPING #-} ElemOf name a ( 'HCons name a as) where
  elemOf = Here

instance
  {-# OVERLAPPABLE #-}
  ( ElemOf name a as
  ) =>
  ElemOf name a ( 'HCons name' b as)
  where
  elemOf :: Elem name a ( 'HCons name' b as)
  elemOf = There (elemOf @name @a @as)

----------------------- PData instances -------------------------------------------------

instance PTryFrom PData (PAsData PInteger) (s :: S) where
  type PTryFromExcess PData (PAsData PInteger) = HSSing "unwrapped" PInteger
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, hsing ver)

instance PTryFrom PData (PAsData PByteString) s where
  type PTryFromExcess PData (PAsData PByteString) = HSSing "unwrapped" PByteString
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, hsing ver)

instance PTryFrom PData PData s where
  type PTryFromExcess PData PData = HRecP 'HNil
  ptryFrom opq = runTermCont $ pure $ (opq, HSNil)

instance PTryFrom PData (PAsData PData) s where
  type PTryFromExcess PData (PAsData PData) = HSSing "unwrapped" PData
  ptryFrom opq = runTermCont $ pure (pdata opq, hsing opq)

-- TODO: add the excess inner type
instance
  ( PTryFrom PData (PAsData a) s
  , PTryFrom PData (PAsData b) s
  ) =>
  PTryFrom PData (PAsData (PBuiltinMap a b)) s
  where
  type PTryFromExcess PData (PAsData (PBuiltinMap a b)) = HSSing "unwrapped" (PBuiltinMap a b)
  ptryFrom opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- TermCont $ ptryFrom @PData @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- TermCont $ ptryFrom @PData @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, hsing ver)

-- TODO: add the excess inner type list
instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinList PData)) s where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = HSSing "unwrapped" (PBuiltinList PData)
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, hsing ver)

instance
  {-# OVERLAPPABLE #-}
  forall a s.
  ( PTryFrom PData (PAsData a) s
  , PIsData a
  ) =>
  PTryFrom PData (PAsData (PBuiltinList (PAsData a))) s
  where
  type PTryFromExcess PData (PAsData (PBuiltinList (PAsData a))) = HSSing "unwrapped" (PBuiltinList (PAsData a))
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pasList # opq
        verify :: Term _ (PData :--> PAsData a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- TermCont $ ptryFrom @PData @(PAsData a) @s $ e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, hsing ver)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData a s
  , a ~ PAsData a'
  , PIsData a'
  , PTryFrom PData b s
  , b ~ PAsData b'
  , PIsData b'
  ) =>
  PTryFrom PData (PAsData (PBuiltinPair a b)) s
  where
  type PTryFromExcess PData (PAsData (PBuiltinPair a b)) = HSSing "unwrapped" (PBuiltinPair a b)
  ptryFrom opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> TermCont (ptryFrom @PData @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> TermCont (ptryFrom @PData @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, hsing ver)

type FromRecordFields :: [PLabeledType] -> RecKind
type family FromRecordFields xs where
  FromRecordFields '[] = 'HNil
  FromRecordFields ((label ':= ptyp) ': xs) = 'HCons label (HSSing "unwrapped" ptyp) (FromRecordFields xs)

type (++) :: RecKind -> RecKind -> RecKind
type family r0 ++ r1 where
  'HNil ++ l = l
  'HCons sym typ l0 ++ l1 = l0 ++ 'HCons sym typ l1

instance
  ( FromRecordFields xs ~ ValidationExcess xs
  , RecordValidation xs s
  ) =>
  PTryFrom PData (PAsData (PDataRecord xs)) (s :: S)
  where
  type
    PTryFromExcess PData (PAsData (PDataRecord xs)) =
      HRecP (HSing "unwrapped" (PDataRecord xs) ++ FromRecordFields xs)

  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pfromData $ punsafeCoerce opq
    (rec', exc) <- recoverRecord @xs @s lst
    rec <- tcont $ plet rec'
    let excess :: PTryFromExcess PData (PAsData (PDataRecord xs)) s
        excess = HSCons @"unwrapped" (MkFlip rec) exc
    pure (punsafeCoerce opq, excess)

class RecordValidation xs s where
  type ValidationExcess xs :: RecKind
  recoverRecord ::
    Term s (PBuiltinList PData) ->
    TermCont s (Term s (PDataRecord xs), HRecP (ValidationExcess xs) s)

instance
  {-# OVERLAPPABLE #-}
  ( RecordValidation xs s
  , PTryFrom PData (PAsData a) s
  , PTryFrom PData (PAsData (PDataRecord xs)) s
  ) =>
  RecordValidation ((label ':= a) ': xs) s
  where
  type ValidationExcess ((label ':= a) ': xs) = 'HCons label (PTryFromExcess PData (PAsData a)) (ValidationExcess xs)
  recoverRecord lst = do
    let lsthead :: Term s PData
        lsthead = phead # lst
        lsttail :: Term s (PBuiltinList PData)
        lsttail = ptail # lst
    (verhead, exchead) <- TermCont $ ptryFrom @PData @(PAsData a) @s lsthead
    (vertail, exctail) <- recoverRecord @xs @s lsttail
    rec <- tcont $ plet $ pdcons @label # verhead # vertail
    pure (rec, HSCons @label exchead exctail)

instance {-# OVERLAPPING #-} RecordValidation '[] s where
  type ValidationExcess '[] = 'HNil
  recoverRecord _ = pure (pdnil, HSNil)

instance
  {-# OVERLAPPING #-}
  forall ys (s :: S).
  ( SumValidation 0 ys s
  ) =>
  PTryFrom PData (PAsData (PDataSum ys)) s
  where
  type PTryFromExcess PData (PAsData (PDataSum ys)) = HRecP 'HNil
  ptryFrom opq = runTermCont $ do
    _ <- tcont $ plet $ validateSum @0 @ys opq
    pure (punsafeCoerce opq, HSNil)

class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) s where
  validateSum :: Term s PData -> Term s (PBuiltinList PData)

instance
  {-# OVERLAPPABLE #-}
  forall (n :: Nat) (x :: [PLabeledType]) (xs :: [[PLabeledType]]) (s :: S).
  ( PTryFrom PData (PAsData (PDataRecord x)) s
  , SumValidation (n + 1) xs s
  , KnownNat n
  , FromRecordFields x ~ ValidationExcess x
  , RecordValidation x s
  ) =>
  SumValidation n (x ': xs) s
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
                  let rec = pdata $ psndBuiltin # elem
                  y <- (getExcessField @"unwrapped" . snd) <$> TermCont (ptryFrom @PData @(PAsData (PDataRecord x)) @s $ pforgetData rec)
                  pure $ punsafeCoerce (y :: Term _ (PDataRecord x))
              )
              (validateSum @(n + 1) @xs $ punsafeCoerce s)
      tcont $ plet snd'

instance {-# OVERLAPPING #-} SumValidation n '[] s where
  validateSum _ = perror

----------------------- POpaque Instances -----------------------------------------------

{- |
    for none of the opaque instances it can be verified
    that the actual structure is what it says to be
    because that data is lost when the PAsData wrapper
    is removed, this can only be safely used if you obtained
    your POpaque safely
-}
instance
  ( PTryFrom PData (PAsData a) s
  , PIsData a
  ) =>
  PTryFrom POpaque a s
  where
  type PTryFromExcess POpaque a = Flip Term (PAsData a)
  ptryFrom opq = runTermCont $ do
    let prop :: Term _ a
        prop = punsafeCoerce opq
    ver' <- fst <$> TermCont (ptryFrom @PData @(PAsData a) $ pforgetData $ pdata prop)
    ver <- tcont $ plet ver'
    pure $ (punsafeCoerce opq, MkFlip ver)

instance
  ( PTryFrom a b s
  , PIsData a
  , PIsData b
  ) =>
  PTryFrom (PAsData a) (PAsData b) s
  where
  type PTryFromExcess (PAsData a) (PAsData b) = PTryFromExcess a b
  ptryFrom opq = runTermCont $ do
    ver' <- snd <$> TermCont (ptryFrom @a @b (pfromData opq))
    pure $ (punsafeCoerce opq, ver')

----------------------- PIsDataReprInstances --------------------------------------------

class PTryFromData s b where 
  type PTryFromDataExcess b :: PType
  type PTryFromDataExcess b = PTryFromExcess PData b
  ptryFromData  :: Term s PData -> ((Term s b, PTryFromDataExcess b s) -> Term s r) -> Term s r
    {-
  default ptryFromData  :: 
    ( PTryFromExcess PData b s ~ PTryFromDataExcess b s 
    , PTryFrom PData b s 
    ) =>
    Term s PData -> ((Term s b, PTryFromDataExcess b s) -> Term s r) -> Term s r
  ptryFromData = ptryFrom @PData @b

deriving anyclass instance (PTryFrom PData b s) => PTryFromData s b
-}

instance 
  ( PTryFrom PData (PAsData (PIsDataReprInstances a)) s
  ) => 
  PTryFromData s (PAsData (PIsDataReprInstances a)) where 
  ptryFromData = ptryFrom 

instance
  ( PIsDataRepr a
  , SumValidation 0 (PIsDataReprRepr a) s
  , PInner a b ~ PDataSum (PIsDataReprRepr a)
  ) =>
  PTryFrom PData (PAsData (PIsDataReprInstances a)) s
  where
  type PTryFromExcess PData (PAsData (PIsDataReprInstances a)) = HRecP 'HNil
  ptryFrom opq = runTermCont $ do
    let reprsum :: Term _ (PDataSum (PIsDataReprRepr a))
        reprsum = pfromData $ unTermCont $ fst <$> TermCont (ptryFrom opq)
    pure $ (pdata $ punsafeFrom reprsum, HSNil)

-- TODO: add overlapping instance for single constructor types that has actual excess 

----------------------- HasField instance -----------------------------------------------

instance (MaybeUnFlip ptyp s out, ElemOf name ptyp rec) => HasField name (HRecP rec s) out where
  getField = getExcessField @name
