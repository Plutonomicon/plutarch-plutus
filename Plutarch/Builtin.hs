module Plutarch.Builtin (
  PData (..),
  pfstBuiltin,
  psndBuiltin,
  pasConstr,
  PBuiltinPair,
  PBuiltinList,
  pTrace,
  (#£),
  (!£),
  singleton,
  hasElem,
  atIndex,
  append,
  cons,
  nil,
  mkList,
  headL,
  PBuiltin (..),
  PList (..),
  PPairData,
) where

import Data.Proxy
import Data.Text (Text)
import Data.Type.Nat
import Plutarch
import Plutarch.Bool
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.String
import qualified PlutusCore as PLC
import qualified PlutusCore.Data as PLC

data PBuiltinPair (a :: k -> Type) (b :: k -> Type) (s :: k)

data PBuiltinList (a :: k -> Type) (s :: k)

data PData s
  = PDataConstr (Term s (PBuiltinPair PInteger (PBuiltinList PData)))
  | PDataMap (Term s (PBuiltinList (PBuiltinPair PData PData)))
  | PDataList (Term s (PBuiltinList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

-- Builtins

{- | Type spec for PLC's untyped builtin

 The `forces` value determines the repeated application of `FORCE` when
 evaluating the builtin function.

 Example: UnConstrData #£ someData
-}
data PBuiltin (forces :: Nat) (args :: [k -> Type]) (res :: k -> Type) where
  UnConstrData :: PBuiltin Nat0 '[POpaque] (PPairData PInteger (PList POpaque))
  UnListData :: PBuiltin Nat0 '[POpaque] (PList POpaque)
  MkPairData :: PBuiltin Nat0 '[a, b] (PPairData a b)
  FstPair :: PBuiltin Nat2 '[PPairData a b] a
  SndPair :: PBuiltin Nat2 '[PPairData a b] b
  MkCons :: PBuiltin Nat1 '[a, PList a] (PList a)
  NullList :: PBuiltin Nat1 '[a] PBool
  HeadList :: PBuiltin Nat1 '[PList a] a
  TailList :: PBuiltin Nat1 '[PList a] (PList a)
  EqualsData :: PBuiltin Nat0 '[POpaque, POpaque] PBool
  IData :: PBuiltin Nat0 '[PInteger] POpaque
  UnIData :: PBuiltin Nat0 '[POpaque] PInteger
  Trace :: PBuiltin Nat1 '[PString, a] a

type family PBuiltinType (args :: [k -> Type]) (res :: k -> Type) where
  PBuiltinType '[] res = res
  PBuiltinType (a ': as) res = a :--> PBuiltinType as res

pBuiltinTerm ::
  forall args res forces s.
  PBuiltin forces args res ->
  Term s (PBuiltinType args res)
pBuiltinTerm b =
  phoistAcyclic $ case b of
    UnConstrData ->
      force @forces Proxy . punsafeBuiltin $ PLC.UnConstrData
    UnListData ->
      force @forces Proxy . punsafeBuiltin $ PLC.UnListData
    MkPairData ->
      force @forces Proxy . punsafeBuiltin $ PLC.MkPairData
    FstPair ->
      force @forces Proxy . punsafeBuiltin $ PLC.FstPair
    SndPair ->
      force @forces Proxy . punsafeBuiltin $ PLC.SndPair
    MkCons ->
      force @forces Proxy . punsafeBuiltin $ PLC.MkCons
    NullList ->
      force @forces Proxy . punsafeBuiltin $ PLC.NullList
    HeadList ->
      force @forces Proxy . punsafeBuiltin $ PLC.HeadList
    TailList ->
      force @forces Proxy . punsafeBuiltin $ PLC.TailList
    EqualsData ->
      force @forces Proxy . punsafeBuiltin $ PLC.EqualsData
    IData ->
      force @forces Proxy . punsafeBuiltin $ PLC.IData
    UnIData ->
      force @forces Proxy . punsafeBuiltin $ PLC.UnIData
    Trace ->
      force @forces Proxy . punsafeBuiltin $ PLC.Trace
  where
    force :: forall forces s a. SNatI forces => Proxy (forces :: Nat) -> Term s a -> Term s a
    force Proxy =
      let sn = snat :: SNat forces
       in forceN (snatToNat sn)
    forceN :: forall s a. Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

(#£) ::
  forall
    k
    (args :: [k -> Type])
    (res :: k -> Type)
    (a :: k -> Type)
    (b :: k -> Type)
    (forces :: Nat)
    (s :: k).
  (PBuiltinType args res ~ (a :--> b)) =>
  PBuiltin forces args res ->
  Term s a ->
  Term s b
(#£) b = (pBuiltinTerm b £)
infixl 9 #£

-- Handy builtin aliases

pTrace :: Text -> Term s a -> Term s a
pTrace s f = Trace #£ pfromText s £ f

(!£) :: forall k (s :: k) (a :: k -> Type). Text -> Term s a -> Term s a
(!£) = pTrace
infixl 8 !£

-- | A builtin list of `Data`.
data PList a s
  = PNil
  | PCons (Term s a) (Term s (PList a))

class ListElemUni (a :: k -> Type) where
  type ListElemType a :: Type
  listElemUni :: Proxy a -> PLC.DefaultUni (PLC.Esc (ListElemType a))

instance ListElemUni PInteger where
  type ListElemType PInteger = Integer
  listElemUni Proxy = PLC.DefaultUniInteger

instance ListElemUni POpaque where
  type ListElemType POpaque = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData

instance ListElemUni (a :: k -> Type) => PlutusType (PList a) where
  type PInner (PList a) _ = PList a
  pcon' PNil =
    punsafeConstant $
      PLC.Some $
        PLC.ValueOf (PLC.DefaultUniProtoList `PLC.DefaultUniApply` listElemUni (Proxy :: Proxy a)) []
  pcon' (PCons x xs) = MkCons #£ x £ xs
  pmatch' = pmatchList

pmatchList :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PList a) -> (PList a s -> Term s b) -> Term s b
pmatchList list f =
  plet (NullList #£ list) $ \isEmpty ->
    pif
      (punsafeCoerce isEmpty)
      (f PNil)
      $ plet
        (HeadList #£ list)
        ( \head ->
            plet (TailList #£ list) $ \tail ->
              f $ PCons head tail
        )

cons :: forall k (s :: k) (a :: k -> Type). ListElemUni a => Term s a -> Term s (PList a) -> Term s (PList a)
cons x xs = pcon' $ PCons x xs

nil :: forall k (s :: k) (a :: k -> Type). ListElemUni a => Term s (PList a)
nil = pcon' PNil

mkList :: forall k (s :: k) (a :: k -> Type). ListElemUni a => [Term s a] -> Term s (PList a)
mkList = \case
  [] -> nil
  (x : xs) -> cons x (mkList xs)

headL :: forall k (s :: k) (c :: k -> Type). Term s (PList c) -> Term s c
headL list =
  pmatchList list $ \case
    PNil -> perror
    PCons x _ -> x

singleton :: ListElemUni a => Term s (a :--> PList a)
singleton =
  plam $ \x ->
    pcon' (PCons x $ pcon' PNil)

hasElem :: (PEq a, ListElemUni a) => ClosedTerm (a :--> PList a :--> PBool)
hasElem =
  pfix £$ plam $ \self k list ->
    pmatch' list $ \case
      PNil ->
        pcon PFalse
      PCons x xs ->
        pif
          (k £== x)
          (pcon PTrue)
          (self £ k £ xs)

atIndex :: ListElemUni a => ClosedTerm (PInteger :--> PList a :--> a)
atIndex =
  pfix £$ plam $ \self n' list ->
    pmatch' ("plu:n" !£ list) $ \case
      PNil ->
        "plu:atIndex:err"
          !£ perror
      PCons x xs ->
        pif
          (n' £== 0)
          x
          (self £ (n' - 1) £ xs)

append :: ListElemUni a => ClosedTerm (PList a :--> PList a :--> PList a)
append =
  pfix £$ plam $ \self list1 list2 ->
    pmatch' ("plu:l1" !£ list1) $ \case
      PNil ->
        list2
      PCons x xs ->
        pcon' (PCons x $ self £ xs £ list2)

data PPairData a b s = PPairData (Term s a) (Term s b)

instance PlutusType (PPairData a b) where
  type PInner (PPairData a b) _ = PPairData a b
  pcon' (PPairData a b) = MkPairData #£ a £ b -- There is no MkPair
  pmatch' pair f =
    -- TODO: use delay/force to avoid evaluating `pair` twice?
    plet (FstPair #£ pair) $ \a ->
      plet (SndPair #£ pair) $ \b ->
        f $ PPairData a b
