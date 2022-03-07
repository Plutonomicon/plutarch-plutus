{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module Plutarch.TryFrom (
  PTryFrom (PTryFromExcess, ptryFrom),
  PFrom (pfrom),
  PMaybeFrom (PMaybeFromExcess, pmaybeFrom),
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+))

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

import Plutarch.Maybe (PMaybe)

import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal.Other (
  POpaque,
  PType,
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
  ptryFrom :: Term s a -> ((Term s b, Term s (PTryFromExcess a b)) -> Term s r) -> Term s r

  -- | this function is only used for `PFrom` and is not exported,
  -- it makes use of PTryFrom being a class that always recovers data
  ptryFromInverse :: Term s b -> Term s a
  ptryFromInverse = punsafeCoerce

----------------------- PData instances -------------------------------------------------

instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = PInteger
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, ver)

instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = PByteString
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, ver)

instance PTryFrom PData PData where
  type PTryFromExcess PData PData = PUnit
  ptryFrom opq = runTermCont $ pure $ (opq, pcon PUnit)

instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinList PData)) where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = PBuiltinList PData
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure $ (punsafeCoerce opq, ver)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData (PAsData a)
  , PTryFrom PData (PAsData b)
  ) =>
  PTryFrom PData (PAsData (PBuiltinMap a b))
  where
  type PTryFromExcess PData (PAsData (PBuiltinMap a b)) = PBuiltinList (PBuiltinPair (PAsData a) (PAsData b))
  ptryFrom opq = runTermCont $ do
    verMap <- tcont $ plet (pasMap # opq)
    -- I've not obtained a `PBuiltinList (PBuiltinPair a b)`
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- TermCont $ ptryFrom @PData @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- TermCont $ ptryFrom @PData @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, ver)

instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinMap POpaque POpaque)) where
  type PTryFromExcess PData (PAsData (PBuiltinMap POpaque POpaque)) = PBuiltinList (PBuiltinPair (PAsData POpaque) (PAsData POpaque))
  ptryFrom opq = runTermCont $ do
    ver <- tcont $ plet (pasMap # opq)
    pure $ (punsafeCoerce opq, punsafeCoerce ver) -- PAsData POpaque ~ PData

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData a
  , a ~ PAsData b
  , PIsData b
  ) =>
  PTryFrom PData (PAsData (PBuiltinList a))
  where
  type PTryFromExcess PData (PAsData (PBuiltinList a)) = PBuiltinList a
  ptryFrom opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList a)
        lst = punsafeBuiltin PLC.UnListData # opq
        verify :: a ~ PAsData b => Term _ (PAsData b :--> a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- TermCont $ ptryFrom @PData @a $ pforgetData e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure $ (punsafeCoerce opq, ver)

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
  type PTryFromExcess PData (PAsData (PBuiltinPair a b)) = PBuiltinPair a b
  ptryFrom opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> TermCont (ptryFrom @PData @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> TermCont (ptryFrom @PData @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure $ (punsafeCoerce opq, ver)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData (PAsData b)
  , PTryFrom PData (PAsData (PDataRecord xs))
  , x ~ (s ':= b)
  , PIsData (PDataRecord xs)
  , PTryFromExcess PData (PAsData (PDataRecord xs)) ~ PDataRecord xs
  ) =>
  PTryFrom PData (PAsData (PDataRecord (x ': xs)))
  where
  type PTryFromExcess PData (PAsData (PDataRecord (x ': xs))) = PDataRecord (x ': xs)
  ptryFrom opq = runTermCont $ do
    lst <- tcont $ plet (pfromData @(PBuiltinList _) $ punsafeCoerce opq)
    let lsthead :: Term _ PData
        lsthead = phead # lst
        lsttail :: Term _ (PAsData (PBuiltinList PData))
        lsttail = pdata $ ptail # lst
        verhead :: Term _ (PAsData b)
        verhead = unTermCont $ fst <$> TermCont (ptryFrom @PData @(PAsData b) lsthead)
        vertail :: Term _ (PDataRecord xs)
        vertail = unTermCont $ snd <$> TermCont (ptryFrom @PData @(PAsData (PDataRecord xs)) (pforgetData lsttail))
    ver <-
      tcont $
        plet $
          pdcons @s
            # verhead
            # vertail
    pure (punsafeCoerce opq, ver)

instance
  {-# OVERLAPPING #-}
  ( PTryFrom PData (PAsData b)
  , PIsData b
  , x ~ (s ':= b)
  ) =>
  PTryFrom PData (PAsData (PDataRecord '[x]))
  where
  type PTryFromExcess PData (PAsData (PDataRecord '[x])) = PDataRecord '[x]
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
    pure $ (punsafeCoerce opq, ver)

class SumValidation (n :: Nat) (sum :: [[PLabeledType]]) where
  validateSum :: Term s PData -> Term s (PBuiltinList PData)

instance
  {-# OVERLAPPABLE #-}
  forall n x xs.
  ( PTryFrom PData (PAsData (PDataRecord x))
  , SumValidation (n + 1) xs
  , KnownNat n
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
                  y <- snd <$> TermCont (ptryFrom @PData @(PAsData (PDataRecord x)) $ pforgetData rec)
                  pure $ punsafeCoerce (y :: Term _ (PTryFromExcess PData (PAsData (PDataRecord x))))
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
  type PTryFromExcess PData (PAsData (PDataSum ys)) = PUnit
  ptryFrom opq = runTermCont $ do
    _ <- tcont $ plet $ validateSum @0 @ys opq
    pure (punsafeCoerce opq, pcon PUnit)

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
  type PTryFromExcess POpaque a = PAsData a
  ptryFrom opq = runTermCont $ do
    let prop :: Term _ a
        prop = punsafeCoerce opq
    ver' <- fst <$> TermCont (ptryFrom @PData @(PAsData a) $ pforgetData $ pdata prop)
    ver <- tcont $ plet ver'
    pure $ (punsafeCoerce opq, ver)

instance
  ( PTryFrom a b
  , PIsData a
  , PIsData b
  ) =>
  PTryFrom (PAsData a) (PAsData b)
  where
  type PTryFromExcess (PAsData a) (PAsData b) = PTryFromExcess a b
  ptryFrom opq = runTermCont $ do
    ver' <- TermCont $ ptryFrom @a @b $ pfromData opq
    ver <- tcont $ plet $ snd ver'
    pure $ (punsafeCoerce opq, ver)

----------------------- The class PFrom -------------------------------------------------

{- |
    Represents infallible conversion.
    Laws:
    - The result type must always carry less information than the origin
      type
-}
class PFrom a b where
  pfrom :: Term s a -> (((Term s b) -> Term s r) -> Term s r)

{- |
    This instance relies on all instances of `PTryFrom` being lawful
-}
instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom b a
  ) =>
  PFrom a b
  where
  pfrom = runTermCont . pure . ptryFromInverse

{-
-- My experimentations around adding `PIsDataRepr` to the
-- machinery

type PFrom :: Constraint -> PType -> PType -> Constraint
class PFrom c a b where
  pfrom :: c => Term s a -> TermCont s (Term s b)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom b a
  ) =>
  PFrom a b
  where
  pfrom = pure . ptryFromInverse
instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PData (PAsData a)
  , c ~ PInner a b
  ) =>
  PTryFrom (PAsData a) c where
    ptryFrom opq = do
      let inner = pto opq
      undefined

instance
  {-# OVERLAPPING #-}
  forall a b c.
  ( PlutusType b
  , c ~ PInner a b
  ) =>
  PFrom (PlutusType b) a c where
  pfrom = pure . pto

-- forall a b. newtype PTryFrom a b => TryFrommable b = MkTryFrommable b

instance
  {-# OVERLAPPABLE #-}
  ( PFamFrom c b a
  ) =>
  PFrom a b
  where
  pfrom = pure . ptryFromInverse

-}

----------------------- The class PMaybeFrom --------------------------------------------

class PMaybeFrom (a :: PType) (b :: PType) where
  type PMaybeFromExcess a b :: PType
  pmaybeFrom :: Term s a -> (((Term s (PMaybe b), Term s (PMaybe (PMaybeFromExcess a b))) -> Term s r) -> Term s r)
