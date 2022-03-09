{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.TryFrom (
  PTryFrom (PTryFromExcess, ptryFrom),
  Flip (Flip, unFlip),
) where

import Data.Kind (Type)
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
  pcon,
  perror,
  plam,
  plet,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Unit (PUnit (PUnit))

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

import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import qualified PlutusCore as PLC

import Plutarch.TermCont (TermCont (TermCont, runTermCont), tcont, unTermCont)

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
class PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  ptryFrom :: Term s a -> ((Term s b, PTryFromExcess a b s) -> Term s r) -> Term s r

----------------------- PData instances -------------------------------------------------

type Flip :: (a -> b -> Type) -> b -> a -> Type
newtype Flip (f :: a -> b -> Type) (y :: b) (x :: a) = Flip {unFlip :: f x y}

instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = Flip Term PInteger
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, Flip ver)

instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = Flip Term PByteString
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, Flip ver)

instance PTryFrom PData PData where
  type PTryFromExcess PData PData = Flip Term PUnit
  ptryFrom opq = runTermCont $ pure $ (opq, Flip $ pcon PUnit)

instance PTryFrom PData (PAsData PData) where
  type PTryFromExcess PData (PAsData PData) = Flip Term PUnit
  ptryFrom opq = runTermCont $ pure (pdata opq, Flip $ pcon PUnit)

instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinList PData)) where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = Flip Term (PBuiltinList PData)
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, Flip ver)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData (PAsData a)
  , PTryFrom PData (PAsData b)
  ) =>
  PTryFrom PData (PAsData (PBuiltinMap a b))
  where
  type PTryFromExcess PData (PAsData (PBuiltinMap a b)) = Flip Term (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))
  ptryFrom opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    -- I've not obtained a `PBuiltinList (PBuiltinPair a b)`
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- TermCont $ ptryFrom @PData @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- TermCont $ ptryFrom @PData @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, Flip ver)

instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinMap POpaque POpaque)) where
  type PTryFromExcess PData (PAsData (PBuiltinMap POpaque POpaque)) = Flip Term (PBuiltinList (PBuiltinPair (PAsData POpaque) (PAsData POpaque)))
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasMap # opq)
    pure $ (punsafeCoerce opq, Flip $ punsafeCoerce ver) -- PAsData POpaque ~ PData

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData a
  , a ~ PAsData b
  , PIsData b
  ) =>
  PTryFrom PData (PAsData (PBuiltinList a))
  where
  type PTryFromExcess PData (PAsData (PBuiltinList a)) = Flip Term (PBuiltinList a)
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList a)
        lst = punsafeBuiltin PLC.UnListData # opq
        verify :: a ~ PAsData b => Term _ (PAsData b :--> a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- TermCont $ ptryFrom @PData @a $ pforgetData e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, Flip ver)

instance
  {-# OVERLAPPABLE #-}
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
        fst' = unTermCont $ fst <$> TermCont (ptryFrom @PData @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> TermCont (ptryFrom @PData @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, Flip ver)

instance
  {-# OVERLAPPABLE #-}
  forall (b :: PType) (xs :: [PLabeledType]) (name :: Symbol) (x :: PLabeledType) (s :: S).
  ( PTryFrom PData (PAsData b)
  , PTryFrom PData (PAsData (PDataRecord xs))
  , x ~ (name ':= b)
  , PIsData (PDataRecord xs)
  , PTryFromExcess PData (PAsData (PDataRecord xs)) s ~ Flip Term (PDataRecord xs) s
  ) =>
  PTryFrom PData (PAsData (PDataRecord (x ': xs)))
  where
  type PTryFromExcess PData (PAsData (PDataRecord (x ': xs))) = Flip Term (PDataRecord (x ': xs))
  ptryFrom opq = runTermCont $ do
    lst <- tcont $ plet (pfromData @(PBuiltinList _) $ punsafeCoerce opq)
    let lsthead :: Term _ PData
        lsthead = phead # lst
        lsttail :: Term _ (PAsData (PBuiltinList PData))
        lsttail = pdata $ ptail # lst
        verhead :: Term _ (PAsData b)
        verhead = unTermCont $ fst <$> TermCont (ptryFrom @PData @(PAsData b) lsthead)
        vertail :: Term _ (PDataRecord xs)
        vertail = unTermCont $ (unFlip . snd) <$> TermCont (ptryFrom @PData @(PAsData (PDataRecord xs)) (pforgetData lsttail))
    ver <-
      tcont $
        plet $
          pdcons @name
            # verhead
            # vertail
    pure (punsafeCoerce opq, Flip ver)

instance
  {-# OVERLAPPING #-}
  ( PTryFrom PData (PAsData b)
  , PIsData b
  , x ~ (s ':= b)
  ) =>
  PTryFrom PData (PAsData (PDataRecord '[x]))
  where
  type PTryFromExcess PData (PAsData (PDataRecord '[x])) = Flip Term (PDataRecord '[x])
  ptryFrom opq = runTermCont $ do
    let lsthead :: Term _ PData
        lsthead = phead # (pfromData @(PBuiltinList _) $ punsafeCoerce opq)
        verhead :: Term _ (PAsData b)
        verhead = unTermCont $ fst <$> TermCont (ptryFrom @PData @(PAsData b) lsthead)
    ver <-
      tcont $
        plet $
          pdcons @s
            # verhead
            # pdnil
    pure $ (punsafeCoerce opq, Flip ver)

class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) where
  validateSum :: Term s PData -> Term s (PBuiltinList PData)

instance
  {-# OVERLAPPABLE #-}
  forall n x xs s.
  ( PTryFrom PData (PAsData (PDataRecord x))
  , SumValidation (n + 1) xs
  , KnownNat n
  , PTryFromExcess PData (PAsData (PDataRecord x)) s ~ Flip Term (PDataRecord x) s
  ) =>
  SumValidation n (x ': xs)
  where
  validateSum s = unTermCont $
    do
      let n :: Integer
          n = natVal (Proxy @n)
      elem <- tcont $ plet $ pasConstr #$ s
      let snd' :: Term _ (PBuiltinList PData)
          snd' =
            pif
              (fromInteger n #== (pfstBuiltin # elem))
              ( unTermCont $ do
                  let rec = pdata $ psndBuiltin # elem
                  y <- (unFlip . snd) <$> TermCont (ptryFrom @PData @(PAsData (PDataRecord x)) $ pforgetData rec)
                  pure $ punsafeCoerce (y :: Term _ (PDataRecord x))
              )
              (validateSum @(n + 1) @xs $ punsafeCoerce s)
      tcont $ plet snd'

instance {-# OVERLAPPING #-} SumValidation n '[] where
  validateSum _ = perror

instance
  {-# OVERLAPPING #-}
  forall ys.
  ( SumValidation 0 ys
  ) =>
  PTryFrom PData (PAsData (PDataSum ys))
  where
  type PTryFromExcess PData (PAsData (PDataSum ys)) = Flip Term PUnit
  ptryFrom opq = runTermCont $ do
    _ <- tcont $ plet $ validateSum @0 @ys opq
    pure (punsafeCoerce opq, Flip $ pcon PUnit)

----------------------- POpaque Instances -----------------------------------------------

{- |
    for none of the opaque instances it can be verified
    that the actual structure is what it says to be
    because that data is lost when the PAsData wrapper
    is removed, this can only be safely used if you obtained
    your POpaque safely
-}
instance
  ( PTryFrom PData (PAsData a)
  , PIsData a
  ) =>
  PTryFrom POpaque a
  where
  type PTryFromExcess POpaque a = Flip Term (PAsData a)
  ptryFrom opq = runTermCont $ do
    let prop :: Term _ a
        prop = punsafeCoerce opq
    ver' <- fst <$> TermCont (ptryFrom @PData @(PAsData a) $ pforgetData $ pdata prop)
    ver <- tcont $ plet ver'
    pure $ (punsafeCoerce opq, Flip ver)

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
