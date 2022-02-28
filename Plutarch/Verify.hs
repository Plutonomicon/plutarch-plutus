{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module Plutarch.Verify (
  PTryFrom (ptryFrom),
  PTryUnwrapFrom (ptryUnwrapFrom),
  PDepth (PDeep, PShallow),
  pcheckType,
  pcheckByteStr,
  pcheckInt,
  pcheckList,
  pcheckMap,
) where
  
-- import GHC.TypeLits (KnownNat)

import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PBuiltinMap,
  PBuiltinPair,
  PData,
  PIsData (pfromData),
  pasByteStr,
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
  perror,
  pforce,
  phoistAcyclic,
  plam,
  plet,
  (#),
  (#$),
  type (:-->),
 )

import Plutarch.DataRepr.Internal (
  PDataRecord,
  PLabeledType ((:=)),
  pdcons,
  pdnil,
 )

import Plutarch.Lift (PLift)

import Plutarch.Bool (pif, (#==))

import Plutarch.List (
  phead,
  pmap,
  ptail,
 )

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
  ptryFrom :: Term s a -> Term s b

----------------------- Polymorphic instances -------------------------------------------

instance PTryFrom a PData (PAsData PInteger) where
  ptryFrom = (pcheckInt #)

instance PTryFrom a PData (PAsData PByteString) where
  ptryFrom = (pcheckByteStr #)

----------------------- PDeep PData instances -------------------------------------------

data PDepth
  = PDeep
  | PShallow

instance PTryFrom PDeep PData PData where
  ptryFrom = id

instance {-# OVERLAPPING #-} PTryFrom PDeep PData (PAsData (PBuiltinList PData)) where
  ptryFrom = (pcheckList #)

instance {-# OVERLAPPING #-} PTryFrom PDeep PData (PAsData (PBuiltinMap PData PData)) where
  ptryFrom = (pcheckMap #)

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PDeep PData a
  , a ~ PAsData b
  , PIsData b
  ) =>
  PTryFrom PDeep PData (PAsData (PBuiltinList a))
  where
  ptryFrom opq = 
      let lst :: Term _ (PBuiltinList a)
          lst = punsafeBuiltin PLC.UnListData # opq
       in pdata $ pmap # (plam $ \e -> ptryFrom @PDeep @PData @a $ pforgetData e) # lst

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
  ptryFrom opq =
      plet (pfromData $ punsafeCoerce opq) $
        \tup ->
          let fst :: Term _ a
              fst = ptryFrom @PDeep @PData @a $ pforgetData $ pfstBuiltin # tup
              snd :: Term _ b
              snd = ptryFrom @PDeep @PData @b $ pforgetData $ psndBuiltin # tup
           in pdata $ ppairDataBuiltin # fst # snd

instance
  {-# OVERLAPPABLE #-}
  ( PTryFrom PDeep PData (PAsData b)
  , PTryFrom PDeep PData (PAsData (PDataRecord xs))
  , x ~ (s ':= b)
  , PIsData (PDataRecord xs)
  ) =>
  PTryFrom PDeep PData (PAsData (PDataRecord (x ': xs)))
  where
  ptryFrom opq = 
    plet (pfromData @(PBuiltinList _) $ punsafeCoerce opq) $ \lst ->
      let lsthead :: Term _ PData
          lsthead = phead # lst
          lsttail :: Term _ (PAsData (PBuiltinList PData))
          lsttail = pdata $ ptail # lst
       in punsafeCoerce $
            pdcons @s
              # (ptryFrom @PDeep @PData @(PAsData b) lsthead)
              # (pfromData (ptryFrom @PDeep @PData @(PAsData (PDataRecord xs)) (pforgetData lsttail)))

instance
  {-# OVERLAPPING #-}
  ( PTryFrom PDeep PData (PAsData b)
  , x ~ (s ':= b)
  ) =>
  PTryFrom PDeep PData (PAsData (PDataRecord '[x]))
  where
  ptryFrom opq =
      let lsthead :: Term _ PData
          lsthead = phead # (pfromData @(PBuiltinList _) $ punsafeCoerce opq)
       in punsafeCoerce $
            pdcons @s
              # (ptryFrom @PDeep @PData @(PAsData b) lsthead)
              # pdnil

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
  ptryFrom opq =
      let prop :: Term _ a
          prop = punsafeCoerce opq
       in pfromData $ ptryFrom @PDeep @PData @(PAsData a) $ pforgetData $ pdata prop

----------------------- PShallow PData instances ----------------------------------------

instance PTryFrom PShallow PData (PAsData (PBuiltinList PData)) where
  ptryFrom = (pcheckList #)

instance PTryFrom PShallow PData (PAsData (PBuiltinMap PData PData)) where
  ptryFrom = (pcheckMap #)

instance PTryFrom PShallow PData PData where
  ptryFrom = id 

-- PShallow POpaque instances wouldn't make sense as that wouldn't do any verifying at all

----------------------- Class that removes the PAsData wrapper --------------------------

{- |
    the basic idea behind the class is, that if i removed the data wrapper, I have
    confidence that the type really has the structure I tested it to have
-}
class PTryUnwrapFrom (d :: PDepth) (b :: PType) where
  ptryUnwrapFrom :: Term s (PData :--> b)

instance PTryUnwrapFrom PDeep PInteger where
  ptryUnwrapFrom = pasInt

instance PTryUnwrapFrom PDeep PByteString where
  ptryUnwrapFrom = pasByteStr

instance {-# OVERLAPPING #-} PTryUnwrapFrom PDeep (PBuiltinList PData) where
  ptryUnwrapFrom = pasList

instance {-# OVERLAPPING #-} PTryUnwrapFrom PDeep (PBuiltinList (PBuiltinPair PData PData)) where
  ptryUnwrapFrom = pasMap

instance
  {-# OVERLAPPABLE #-}
  ( PTryUnwrapFrom PDeep b
  , PIsData b
  , PLift b
  ) =>
  PTryUnwrapFrom PDeep (PBuiltinList b)
  where
  ptryUnwrapFrom = phoistAcyclic $
    plam $ \opq ->
      let lst :: Term _ (PBuiltinList (PAsData b))
          lst = punsafeBuiltin PLC.UnListData # opq
       in pmap # (plam $ \e -> ptryUnwrapFrom @PDeep @b #$ pforgetData e) # lst

-- this instance is not ok, I can not create an instance that doesn't work on
-- wrapped types.
instance
  {-# OVERLAPPABLE #-}
  ( PTryUnwrapFrom PDeep a
  , PTryUnwrapFrom PDeep b
  , PIsData a
  , PIsData b
  ) =>
  PTryUnwrapFrom PDeep (PBuiltinPair (PAsData a) (PAsData b))
  where
  ptryUnwrapFrom = phoistAcyclic $
    plam $ \opq ->
      let tup :: Term _ (PBuiltinPair (PAsData a) (PAsData b))
          tup = pfromData $ punsafeCoerce opq
          fst :: Term _ a
          fst = ptryUnwrapFrom @PDeep @a #$ pforgetData $ pfstBuiltin # tup
          snd :: Term _ b
          snd = ptryUnwrapFrom @PDeep @b #$ pforgetData $ psndBuiltin # tup
       in ppairDataBuiltin # pdata fst # pdata snd

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
