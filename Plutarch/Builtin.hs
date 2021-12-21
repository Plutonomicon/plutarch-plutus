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
  PBuiltin (..),
  PList (..),
  PPair (..),
) where

import Data.Proxy
import Data.Text (Text)
import Data.Type.Nat
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin.List.Type
import Plutarch.Builtin.Pair.Type
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.String
import qualified PlutusCore as PLC

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
  UnConstrData :: PBuiltin Nat0 '[POpaque] (PPair PInteger (PList POpaque))
  UnListData :: PBuiltin Nat0 '[POpaque] (PList POpaque)
  MkPairData :: PBuiltin Nat0 '[a, b] (PPair a b)
  FstPair :: PBuiltin Nat2 '[PPair a b] a
  SndPair :: PBuiltin Nat2 '[PPair a b] b
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
