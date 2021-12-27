module Plutarch.Internal ((:-->), PDelayed, Term, plam', plet, papp, pdelay, pforce, phoistAcyclic, perror, punsafeCoerce, punsafeBuiltin, punsafeConstant, compile, ClosedTerm, Dig, hashTerm, hashOpenTerm, TermCont (..)) where

import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (Blake2b_160)
import Crypto.Hash.IO (HashAlgorithm)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust)
import qualified Flat.Run as F
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore (Some, ValueOf)
import qualified PlutusCore as PLC
import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
import qualified UntypedPlutusCore as UPLC

-- Explanation for hoisted terms:
-- Hoisting is a convenient way of importing terms without duplicating them
-- across your tree. Currently, hoisting is only supported on terms that do
-- not refer to any free variables.
--
-- An RHoisted contains a term and its hash. A RawTerm will have a DAG
-- of hoisted terms, where an edge represents a dependency.
-- We topologically sort these hoisted terms, such that each has an index.
--
-- We wrap our RawTerm in RLamAbs and RApply in an order corresponding to the
-- indices. Each level can refer to levels above it by the nature of De Bruijn naming,
-- though the name is relative to the current level.

type Dig = Digest Blake2b_160

data HoistedTerm = HoistedTerm Dig RawTerm

data RawTerm
  = RVar Natural
  | RLamAbs RawTerm
  | RApply RawTerm RawTerm
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RError
  | RHoisted HoistedTerm

hashRawTerm' :: HashAlgorithm alg => RawTerm -> Context alg -> Context alg
hashRawTerm' (RVar x) = flip hashUpdate ("0" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral x :: Integer))
hashRawTerm' (RLamAbs x) = flip hashUpdate ("1" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RApply x y) = flip hashUpdate ("2" :: BS.ByteString) . hashRawTerm' x . hashRawTerm' y
hashRawTerm' (RForce x) = flip hashUpdate ("3" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RDelay x) = flip hashUpdate ("4" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RConstant x) = flip hashUpdate ("5" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' (RBuiltin x) = flip hashUpdate ("6" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' RError = flip hashUpdate ("7" :: BS.ByteString)
hashRawTerm' (RHoisted (HoistedTerm hash _)) = flip hashUpdate ("8" :: BS.ByteString) . flip hashUpdate hash

hashRawTerm :: RawTerm -> Dig
hashRawTerm t = hashFinalize . hashRawTerm' t $ hashInit

-- Source: Unembedding Domain-Specific Languages by Robert Atkey, Sam Lindley, Jeremy Yallop
-- Thanks!
-- NB: Hoisted terms must be sorted such that the dependents are first and dependencies last.
--
-- s: This parameter isn't ever instantiated with something concrete. It is merely here
-- to ensure that `compile` and `phoistAcyclic` only accept terms without any free variables.
--
-- Explanation of how the unembedding works:
-- Each term must be instantiated with its de-Bruijn level.
-- `plam'`, given its own level, will create an `RVar` that figures out the
-- de-Bruijn index needed to reach its own level given the level it itself is
-- instantiated with.
newtype Term (s :: k) (a :: k -> Type) = Term {asRawTerm :: Natural -> (RawTerm, [HoistedTerm])}

type ClosedTerm (a :: k -> Type) = forall (s :: k). Term s a

data (:-->) (a :: k -> Type) (b :: k -> Type) (s :: k)
infixr 0 :-->
data PDelayed (a :: k -> Type) (s :: k)

plam' :: (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ \i ->
  let v = Term $ \j -> (RVar (j - (i + 1)), [])
      (t, deps) = asRawTerm (f v) (i + 1)
   in (RLamAbs $ t, deps)

-- TODO: This implementation is ugly. Perhaps Term should be different?
plet :: Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ \i -> case asRawTerm v i of
  -- Avoid double lets
  (RVar _, _) -> asRawTerm (f v) i
  _ -> asRawTerm (papp (plam' f) v) i

papp :: Term s (a :--> b) -> Term s a -> Term s b
papp x y = Term $ \i ->
  let (x', deps) = asRawTerm x i
   in let (y', deps') = asRawTerm y i
       in (RApply x' y', deps ++ deps')

pdelay :: Term s a -> Term s (PDelayed a)
pdelay x = Term $ \i ->
  let (x', deps) = asRawTerm x i
   in (RDelay x', deps)

pforce :: Term s (PDelayed a) -> Term s a
pforce x = Term $ \i ->
  let (x', deps) = asRawTerm x i
   in (RForce x', deps)

perror :: Term s a
perror = Term $ \_ -> (RError, [])

punsafeCoerce :: Term s a -> Term s b
punsafeCoerce (Term x) = Term x

punsafeBuiltin :: UPLC.DefaultFun -> Term s a
punsafeBuiltin f = Term $ \_ -> (RBuiltin f, [])

punsafeConstant :: Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstant c = Term $ \_ -> (RConstant c, [])

-- FIXME: Give proper error message when mutually recursive.
phoistAcyclic :: ClosedTerm a -> Term s a
phoistAcyclic t = Term $ \_ ->
  let (t', deps) = asRawTerm t 0
   in let t'' = HoistedTerm (hashRawTerm t') t'
       in (RHoisted t'', t'' : deps)

rawTermToUPLC :: (HoistedTerm -> Natural) -> Natural -> RawTerm -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC _ _ (RVar i) = UPLC.Var () (DeBruijn . Index $ i + 1) -- Why the fuck does it start from 1 and not 0?
rawTermToUPLC m l (RLamAbs t) = UPLC.LamAbs () (DeBruijn . Index $ 0) (rawTermToUPLC m (l + 1) t)
rawTermToUPLC m l (RApply x y) = UPLC.Apply () (rawTermToUPLC m l x) (rawTermToUPLC m l y)
rawTermToUPLC m l (RDelay t) = UPLC.Delay () (rawTermToUPLC m l t)
rawTermToUPLC m l (RForce t) = UPLC.Force () (rawTermToUPLC m l t)
rawTermToUPLC _ _ (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC _ _ (RConstant c) = UPLC.Constant () c
rawTermToUPLC _ _ RError = UPLC.Error ()
rawTermToUPLC m l (RHoisted hoisted) = UPLC.Var () . DeBruijn . Index $ l - m hoisted

compile' :: ClosedTerm a -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
compile' t =
  let (t', deps) = asRawTerm t 0

      f :: Natural -> Maybe Natural -> (Bool, Maybe Natural)
      f n Nothing = (True, Just n)
      f _ (Just n) = (False, Just n)

      g :: HoistedTerm -> (M.Map Dig Natural, [(Natural, RawTerm)], Natural) -> (M.Map Dig Natural, [(Natural, RawTerm)], Natural)
      g (HoistedTerm hash term) (map, defs, n) = case M.alterF (f n) hash map of
        (True, map) -> (map, (n, term) : defs, n + 1)
        (False, map) -> (map, defs, n)

      -- map: term -> de Bruijn level
      -- defs: the terms, level 0 is last
      -- n: # of terms
      (map, defs, n) = foldr g (M.empty, [], 0) deps

      map' = fromJust . flip M.lookup map . (\(HoistedTerm hash _) -> hash)

      body = rawTermToUPLC map' n t'

      wrapped = foldl' (\b (lvl, def) -> UPLC.Apply () (UPLC.LamAbs () (DeBruijn . Index $ 0) b) (rawTermToUPLC map' lvl def)) body defs
   in wrapped

compile :: ClosedTerm a -> Script
compile t = Script $ UPLC.Program () (PLC.defaultVersion ()) (compile' t)

newtype TermCont s a = TermCont {runTermCont :: forall b. (a -> Term s b) -> Term s b}

instance Functor (TermCont s) where
  fmap f (TermCont g) = TermCont $ \h -> g (h . f)

instance Applicative (TermCont s) where
  pure x = TermCont $ \f -> f x
  x <*> y = do
    x <- x
    y <- y
    pure (x y)

instance Monad (TermCont s) where
  (TermCont f) >>= g = TermCont $ \h ->
    f
      ( \x ->
          runTermCont (g x) h
      )

hashTerm :: ClosedTerm a -> Dig
hashTerm t =
  let (t', _) = asRawTerm t 0
   in hashRawTerm t'

hashOpenTerm :: Term s a -> TermCont s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i ->
  let inner = f $ hashRawTerm . fst $ asRawTerm x i
   in asRawTerm inner i
