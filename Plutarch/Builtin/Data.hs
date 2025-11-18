{-
BuiltinPair and BuiltinList should go into their own module !!
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Data (
  PData (PData),
  PAsData (PAsData),
  pchooseData,
  pasConstr,
  pasMap,
  plistData,
  pasList,
  pasInt,
  pasByteStr,
  pserialiseData,
  pconstrBuiltin,
  PBuiltinPair (PBuiltinPair),
  pfstBuiltin,
  psndBuiltin,
  ppairDataBuiltin,
  PBuiltinList (PCons, PNil),
  pheadBuiltin,
  ptailBuiltin,
  pheadTailBuiltin,
  pchooseListBuiltin,
  pnullBuiltin,
  pconsBuiltin,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (
  RawTerm (RCase),
  S,
  Term (Term),
  TermResult (TermResult),
  asRawTerm,
  pforce,
  phoistAcyclic,
  punsafeBuiltin,
  (:-->),
 )
import PlutusCore qualified as PLC

newtype PData (s :: S) = PData (Term s PData)

newtype PAsData (a :: S -> Type) (s :: S) = PAsData (Term s a)

pchooseData :: Term s (PData :--> a :--> a :--> a :--> a :--> a :--> a)
pchooseData = phoistAcyclic $ pforce $ punsafeBuiltin PLC.ChooseData

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

pasMap :: Term s (PData :--> PBuiltinList (PBuiltinPair PData PData))
pasMap = punsafeBuiltin PLC.UnMapData

plistData :: Term s (PBuiltinList PData :--> PData)
plistData = punsafeBuiltin PLC.ListData

pasList :: Term s (PData :--> PBuiltinList PData)
pasList = punsafeBuiltin PLC.UnListData

pasInt :: Term s (PData :--> PInteger)
pasInt = punsafeBuiltin PLC.UnIData

pasByteStr :: Term s (PData :--> PByteString)
pasByteStr = punsafeBuiltin PLC.UnBData

-- | Serialise any builtin data to its cbor represented by a builtin bytestring
pserialiseData :: Term s (PData :--> PByteString)
pserialiseData = punsafeBuiltin PLC.SerialiseData

pconstrBuiltin :: Term s (PInteger :--> PBuiltinList PData :--> PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
pconstrBuiltin = punsafeBuiltin PLC.ConstrData

--------------------------------------------------------------------------------

{- | A builtin Plutus pair.

@since 1.12.0
-}
data PBuiltinPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBuiltinPair (Term s a) (Term s b)

{-# DEPRECATED pfstBuiltin "Use pmatch instead" #-}
pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

{-# DEPRECATED psndBuiltin "Use pmatch instead" #-}
psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

{- | Construct a builtin pair of 'PData' elements.

Uses 'PAsData' to preserve more information about the underlying 'PData'.
-}
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
ppairDataBuiltin = punsafeBuiltin PLC.MkPairData

--------------------------------------------------------------------------------

-- | Plutus 'BuiltinList'
data PBuiltinList (a :: S -> Type) (s :: S)
  = PCons (Term s a) (Term s (PBuiltinList a))
  | PNil

pheadBuiltin :: Term s (PBuiltinList a :--> a)
pheadBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.HeadList

ptailBuiltin :: Term s (PBuiltinList a :--> PBuiltinList a)
ptailBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.TailList

{- | Use this in preference to 'pheadBuiltin' and 'ptailBuiltin' on the same
'PBuiltinList', as this will be faster. This is also faster than a 'pmatch',
as the 'PNil' case is omitted.

@since wip
-}
pheadTailBuiltin ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PBuiltinList a) ->
  (Term s a -> Term s (PBuiltinList a) -> Term s b) ->
  Term s b
pheadTailBuiltin ell handler = Term $ \level -> do
  TermResult matchRaw matchDeps <- asRawTerm (plam handler) level
  TermResult rawT depsT <- asRawTerm ell level
  let allDeps = matchDeps <> depsT
  pure . TermResult (RCase rawT [matchRaw]) $ allDeps

pchooseListBuiltin :: Term s (PBuiltinList a :--> b :--> b :--> b)
pchooseListBuiltin = phoistAcyclic $ pforce $ pforce $ punsafeBuiltin PLC.ChooseList

pnullBuiltin :: Term s (PBuiltinList a :--> PBool)
pnullBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.NullList

pconsBuiltin :: Term s (a :--> PBuiltinList a :--> PBuiltinList a)
pconsBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin PLC.MkCons
