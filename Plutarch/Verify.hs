{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module Plutarch.Verify (
  PTryFrom (ptryFrom),
  PDepth (PDeep, PShallow),
  pcheckType,
  pcheckByteStr,
  pcheckInt,
  pcheckList,
  pcheckMap,
) where

import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PBuiltinMap,
  PBuiltinPair,
  PData,
  PIsData (pfromData),
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
  perror,
  pforce,
  phoistAcyclic,
  plam,
  (#),
  (#$),
  type (:-->),
 )

import Plutarch.Bool (pif, (#==))

import Plutarch.List (pmap)

import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import qualified PlutusCore as PLC

{- |
    Note: PAsData POpaque ~ PData
-}

----------------------- The class PTryFrom ----------------------------------------------

{- |
    This checks the datastructure for validity.
    Be aware that if called with `PDeep`, it will in
    most cases will at least be slightly more expensive
    Be aware this might get really expensive, so only
    use it if you cannot establish trust otherwise
    (e.g. via only checking a part of your Data with
    PTryFrom)
-}
class PTryFrom (d :: PDepth) (a :: PType) (b :: PType) where
  --      this is the "safest" type --^            ^-- this is the target type
  ptryFrom :: Term s (a :--> b)

----------------------- Polymorphic instances -------------------------------------------

instance PTryFrom a PData (PAsData PInteger) where
  ptryFrom = pcheckInt

instance PTryFrom a PData (PAsData PByteString) where
  ptryFrom = pcheckByteStr

----------------------- PDeep PData instances -------------------------------------------

data PDepth
  = PDeep
  | PShallow

{-
instance {-# OVERLAPPING #-} PTryFrom PDeep PData PData where
  ptryFrom = plam id
-}

instance {-# OVERLAPPING #-} PTryFrom PDeep PData (PAsData (PBuiltinList PData)) where
  ptryFrom = pcheckList

instance {-# OVERLAPPING #-} PTryFrom PDeep PData (PAsData (PBuiltinMap PData PData)) where
  ptryFrom = pcheckMap

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PDeep PData a
  , a ~ PAsData b
  , PIsData b
  ) =>
  PTryFrom PDeep PData (PAsData (PBuiltinList a))
  where
  ptryFrom = phoistAcyclic $
    plam $ \opq ->
      let lst :: Term _ (PBuiltinList a)
          lst = punsafeBuiltin PLC.UnListData # opq
       in pdata $ pmap # (plam $ \e -> ptryFrom @PDeep @PData @a #$ pforgetData e) # lst

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PDeep PData a
  , a ~ PAsData a'
  , PIsData a'
  , PTryFrom PDeep PData b
  , b ~ PAsData b'
  , PIsData b'
  ) =>
  PTryFrom PDeep PData (PAsData (PBuiltinPair a b))
  where
  ptryFrom = phoistAcyclic $
    plam $ \opq ->
      let tup :: Term _ (PBuiltinPair a b)
          tup = pfromData $ punsafeCoerce opq
          fst :: Term _ a
          fst = ptryFrom @PDeep @PData @a #$ pforgetData $ pfstBuiltin # tup
          snd :: Term _ b
          snd = ptryFrom @PDeep @PData @b #$ pforgetData $ psndBuiltin # tup
       in pdata $ ppairDataBuiltin # fst # snd

----------------------- PDeep POpaque instances -----------------------------------------

{- |
    for none of the opaque instances it can be verified
    that the actual structure is what it says to be
    because that data is lost when the PAsData wrapper
    is removed, this can only be safely used if you obtained
    your POpaque safely
-}
instance
  ( PTryFrom PDeep PData (PAsData a)
  , PIsData a
  ) =>
  PTryFrom PDeep POpaque a
  where
  ptryFrom = phoistAcyclic $
    plam $ \opq ->
      let prop :: Term _ a
          prop = punsafeCoerce opq
       in pfromData $ ptryFrom @PDeep @PData @(PAsData a) #$ pforgetData $ pdata prop

----------------------- PShallow PData instances ----------------------------------------

instance PTryFrom PShallow PData (PAsData (PBuiltinList PData)) where
  ptryFrom = pcheckList

instance PTryFrom PShallow PData (PAsData (PBuiltinMap PData PData)) where
  ptryFrom = pcheckMap

instance PTryFrom PShallow PData PData where
  ptryFrom = plam id

-- PShallow POpaque instances wouldn't make sense as that wouldn't do any verifying at all

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
