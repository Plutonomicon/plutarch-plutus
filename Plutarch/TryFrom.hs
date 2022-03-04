{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module Plutarch.TryFrom (
  PTryFrom (PTryFromExcess, ptryFrom),
  pcheckType,
  pcheckByteStr,
  pcheckInt,
  pcheckList,
  pcheckMap,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+))

-- import Data.Kind (Constraint)

import Plutarch.Builtin (
  PAsData,
  PBuiltinList (PCons, PNil),
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
  Term,
  pcon,
  perror,
  pfix,
  pmatch,
  pforce,
  phoistAcyclic,
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

import Plutarch.TermCont (TermCont, tcont, unTermCont)

{- |
    Note: PAsData POpaque ~ PData
-}

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
-}
class PTryFrom (a :: PType) (b :: PType) where
  --     "safest" type --^            ^-- target type
  type PTryFromExcess a b :: PType
  ptryFrom :: Term s a -> TermCont s (Term s b, Term s (PTryFromExcess a b))

----------------------- PData instances -------------------------------------------------

instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = PInteger
  ptryFrom opq = do
    ver <- tcont $ plet (pasInt # opq)
    pure $ (punsafeCoerce opq, ver)

instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = PByteString
  ptryFrom opq = do
    ver <- tcont $ plet (pasByteStr # opq)
    pure $ (punsafeCoerce opq, ver)

instance PTryFrom PData PData where
  type PTryFromExcess PData PData = PUnit
  ptryFrom opq = pure $ (opq, pcon PUnit)

instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinList PData)) where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = PBuiltinList PData
  ptryFrom opq = do
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
  ptryFrom opq = do
    verMap <- tcont $ plet (pasMap # opq)
    -- I've not obtained a `PBuiltinList (PBuiltinPair a b)`
    let verifyPair :: Term _ (PBuiltinPair PData PData :--> PBuiltinPair (PAsData a) (PAsData b))
        verifyPair = plam $ \tup -> unTermCont $ do
          (verfst, _) <- ptryFrom @PData @(PAsData a) $ pfstBuiltin # tup
          (versnd, _) <- ptryFrom @PData @(PAsData b) $ psndBuiltin # tup
          pure $ ppairDataBuiltin # verfst # versnd
    ver <- tcont $ plet $ pmap # verifyPair # verMap
    pure (punsafeCoerce opq, ver)

instance {-# OVERLAPPING #-} PTryFrom PData (PAsData (PBuiltinMap POpaque POpaque)) where
  type PTryFromExcess PData (PAsData (PBuiltinMap POpaque POpaque)) = PBuiltinList (PBuiltinPair (PAsData POpaque) (PAsData POpaque))
  ptryFrom opq = do
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
  ptryFrom opq =
    let lst :: Term _ (PBuiltinList a)
        lst = punsafeBuiltin PLC.UnListData # opq
        verify :: a ~ PAsData b => Term _ (PAsData b :--> a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- ptryFrom @PData @a $ pforgetData e
            pure wrapped
     in do
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
  ptryFrom opq = do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> (ptryFrom @PData @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> (ptryFrom @PData @b $ pforgetData $ psndBuiltin # tup)
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
  ptryFrom opq = do
    lst <- tcont $ plet (pfromData @(PBuiltinList _) $ punsafeCoerce opq)
    let lsthead :: Term _ PData
        lsthead = phead # lst
        lsttail :: Term _ (PAsData (PBuiltinList PData))
        lsttail = pdata $ ptail # lst
        verhead :: Term _ (PAsData b)
        verhead = unTermCont $ fst <$> (ptryFrom @PData @(PAsData b) lsthead)
        vertail :: Term _ (PDataRecord xs)
        vertail = unTermCont $ snd <$> (ptryFrom @PData @(PAsData (PDataRecord xs)) (pforgetData lsttail))
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
  ptryFrom opq = do
    let lsthead :: Term _ PData
        lsthead = phead # (pfromData @(PBuiltinList _) $ punsafeCoerce opq)
        verhead :: Term _ (PAsData b)
        verhead = unTermCont $ fst <$> (ptryFrom @PData @(PAsData b) lsthead)
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
                  y <- snd <$> (ptryFrom @PData @(PAsData (PDataRecord x)) $ pforgetData rec)
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
  -- , AllRecordsPTryFrom ys
  ) =>
  PTryFrom PData (PAsData (PDataSum ys))
  where
  type PTryFromExcess PData (PAsData (PDataSum ys)) = PUnit
  ptryFrom opq = do
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
  ptryFrom opq = do
    let prop :: Term _ a
        prop = punsafeCoerce opq
    ver' <- fst <$> (ptryFrom @PData @(PAsData a) $ pforgetData $ pdata prop)
    ver <- tcont $ plet ver'
    pure $ (punsafeCoerce opq, ver)

----------------------- Helper functions ------------------------------------------------

pchooseData :: Term s (PData :--> a :--> a :--> a :--> a :--> a :--> a)
pchooseData = phoistAcyclic $ pforce $ punsafeBuiltin PLC.ChooseData

pcheckType :: (Term s PInteger) -> Term _ (PData :--> PAsData b)
pcheckType i = plam $ \d ->
  let con :: Term _ PInteger
      con = pchooseData # d # 0 # 1 # 2 # 3 # 4
   in pif (con #== i) (punsafeCoerce d) perror

pcheckMap :: Term s (PData :--> PAsData (PBuiltinMap PData PData))
pcheckMap = pcheckType 1

pcheckList :: Term s (PData :--> PAsData (PBuiltinList PData))
pcheckList = pcheckType 2

pcheckInt :: Term s (PData :--> PAsData PInteger)
pcheckInt = pcheckType 3

pcheckByteStr :: Term s (PData :--> PAsData PByteString)
pcheckByteStr = pcheckType 4
