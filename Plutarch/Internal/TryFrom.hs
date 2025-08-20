{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.TryFrom (
  PTryFrom,
  PTryFromExcess,
  ptryFrom',
  ptryFrom,
  PSubtypeRelation (PSubtypeRelation, PNoSubtypeRelation),
  PSubtype,
  PSubtype',
  pupcast,
  pupcastF,
  pdowncastF,
) where

import Data.Functor.Const (Const)
import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (PBool, pif, (#||))
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PBuiltinPair,
  PData,
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pfstBuiltin,
  ppairDataBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Integer (
  PInteger,
  pconstantInteger,
  peqInteger,
 )
import Plutarch.Builtin.String (ptraceInfo)
import Plutarch.Internal.IsData (
  PIsData,
  pdata,
  pforgetData,
  pfromData,
 )
import Plutarch.Internal.ListLike (PListLike (pnull), pmap)
import Plutarch.Internal.Numeric (PNatural, PPositive, ptryNatural, ptryPositive)
import Plutarch.Internal.PLam (PLamN (plam))
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Subtype (
  PSubtype,
  PSubtype',
  PSubtypeRelation (PNoSubtypeRelation, PSubtypeRelation),
  pdowncastF,
  pupcast,
  pupcastF,
 )
import Plutarch.Internal.Term (
  PType,
  Term,
  perror,
  plet,
  punsafeCoerce,
  (#),
  type (:-->),
 )
import Plutarch.Internal.TermCont (runTermCont, tcont, unTermCont)
import Plutarch.Reducible (Reduce)

{- |
@PTryFrom a b@ represents a subtyping relationship between @a@ and @b@,
and a way to go from @a@ to @b@.
Laws:
- @(punsafeCoerce . fst) <$> tcont (ptryFrom x) ≡ pure x@
-}
class PSubtype a b => PTryFrom (a :: PType) (b :: PType) where
  type PTryFromExcess a b :: PType
  type PTryFromExcess a b = PTryFromExcess a (PInner b)
  ptryFrom' :: forall s r. Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
  default ptryFrom' :: forall s r. (PTryFrom a (PInner b), PTryFromExcess a b ~ PTryFromExcess a (PInner b)) => Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
  ptryFrom' opq f = ptryFrom @(PInner b) @a opq \(inn, exc) -> f (punsafeCoerce inn, exc)

ptryFrom :: forall b a s r. PTryFrom a b => Term s a -> ((Term s b, Reduce (PTryFromExcess a b s)) -> Term s r) -> Term s r
ptryFrom = ptryFrom'

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PInteger) where
  type PTryFromExcess PData (PAsData PInteger) = Flip Term PInteger
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasInt # opq)
    pure (punsafeCoerce opq, ver)

instance PTryFrom PData (PAsData PByteString) where
  type PTryFromExcess PData (PAsData PByteString) = Flip Term PByteString
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasByteStr # opq)
    pure (punsafeCoerce opq, ver)

{- |
    This verifies a list to be indeed a list but doesn't recover the inner data
    use this instance instead of the one for `PData (PAsData (PBuiltinList (PAsData a)))`
    as this is O(1) instead of O(n)
-}

-- TODO: add the excess inner type list
instance PTryFrom PData (PAsData (PBuiltinList PData)) where
  type PTryFromExcess PData (PAsData (PBuiltinList PData)) = Flip Term (PBuiltinList PData)
  ptryFrom' opq = runTermCont $ do
    ver <- tcont $ plet (pasList # opq)
    pure (punsafeCoerce opq, ver)

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
  ptryFrom' opq = runTermCont $ do
    let lst :: Term _ (PBuiltinList PData)
        lst = pasList # opq
        verify :: Term _ (PData :--> PAsData a)
        verify = plam $ \e ->
          unTermCont $ do
            (wrapped, _) <- tcont $ ptryFrom @(PAsData a) $ e
            pure wrapped
    ver <- tcont $ plet $ pmap # verify # lst
    pure (punsafeCoerce opq, ver)

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
  ptryFrom' opq = runTermCont $ do
    tup <- tcont $ plet (pfromData $ punsafeCoerce opq)
    let fst' :: Term _ a
        fst' = unTermCont $ fst <$> tcont (ptryFrom @a $ pforgetData $ pfstBuiltin # tup)
        snd' :: Term _ b
        snd' = unTermCont $ fst <$> tcont (ptryFrom @b $ pforgetData $ psndBuiltin # tup)
    ver <- tcont $ plet $ ppairDataBuiltin # fst' # snd'
    pure (punsafeCoerce opq, ver)

-- | @since 1.7.0
instance PTryFrom PData (PAsData PBool) where
  type PTryFromExcess PData (PAsData PBool) = Const ()
  ptryFrom' opq = runTermCont $ do
    asConstr <- tcont . plet $ pasConstr # opq
    let ix = pfstBuiltin # asConstr
    tcont $ \f ->
      pif
        ((peqInteger # ix # pconstantInteger 0) #|| (peqInteger # ix # pconstantInteger 1))
        (f ())
        (ptraceInfo "PTryFrom(PAsData PBool): invalid constructor tag" perror)
    let dat = psndBuiltin # asConstr
    tcont $ \f ->
      pif
        (pnull # dat)
        (f ())
        (ptraceInfo "PTryFrom(PAsData PBool): non-empty constructor list" perror)
    pure (punsafeCoerce opq, ())

instance PTryFrom PData (PAsData PData) where
  type PTryFromExcess PData (PAsData PData) = Const ()
  ptryFrom' opq = runTermCont $ pure (pdata opq, ())

instance PTryFrom PData PData where
  type PTryFromExcess PData PData = Const ()
  ptryFrom' opq f = f (opq, ())

-- | @since 1.10.0
instance PTryFrom PInteger PPositive where
  type PTryFromExcess PInteger PPositive = Const ()
  ptryFrom' opq = runTermCont $ pure (ptryPositive # opq, ())

-- | @since 1.10.0
instance PTryFrom PData (PAsData PPositive) where
  type PTryFromExcess PData (PAsData PPositive) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
    res <- tcont . plet $ ptryPositive # i
    resData <- tcont . plet $ pdata res
    pure (resData, res)

-- | @since 3.4.0
instance PTryFrom PData (PAsData PNatural) where
  ptryFrom' opq = runTermCont $ do
    (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
    res <- tcont . plet $ ptryNatural # i
    resData <- tcont . plet $ pdata res
    pure (resData, ())
