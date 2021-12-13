{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch
  ( PI.Constant(..)
  , (PI.:-->)
  , PI.PDelayed
  , PI.Term
  , PI.plam
  , PI.papp
  , PI.pdelay
  , PI.pforce
  , PI.phoistAcyclic
  , PI.perror
  , PI.punsafeCoerce
  , PI.punsafeBuiltin
  , PI.punsafeConstant
  , PI.compile
  , PI.ClosedTerm
  , PlutusType(..)
  , printTerm
  , (£$)
  , (£)
  , plam2
  , plam3
  , plam4
  , plam5
  , plet
  , pinl
  , pcon
  , pmatch
  , punsafeFrom
  , pto
  , pfix
  , POpaque(..)
  , popaque
  , punsafeFromOpaque
) where
  
import Plutarch.Internal (Term, papp, punsafeCoerce, (:-->), plam, compile, phoistAcyclic, ClosedTerm)
import Plutus.V1.Ledger.Scripts (Script(Script))
import qualified Plutarch.Internal as PI
import PlutusCore.Pretty (prettyPlcReadableDebug)
import Data.Kind (Type)

-- TODO: Heavily improve. It's unreadable right now.
-- We could convert the de Bruijn indices into names.
-- show . prettyPlcReadableDef . (\(Right p) -> p) . Scripts.mkTermToEvaluate . compile $ term
printTerm :: ClosedTerm a -> String
printTerm term = show . prettyPlcReadableDebug . (\(Script s) -> s) $ compile term

(£) :: Term s (a :--> b) -> Term s a -> Term s b
(£) = papp
infixl 8 £

(£$) :: Term s (a :--> b) -> Term s a -> Term s b
(£$) = papp
infixr 0 £$

-- TODO: Replace with plamN

plam2 :: (Term s a -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
plam2 f = plam $ \x -> plam $ \y -> f x y

plam3 :: (Term s a -> Term s b -> Term s c -> Term s d) -> Term s (a :--> b :--> c :--> d)
plam3 f = plam $ \x -> plam $ \y -> plam $ \z -> f x y z

plam4 :: (Term s a -> Term s b -> Term s c -> Term s d -> Term s e) -> Term s (a :--> b :--> c :--> d :--> e)
plam4 f = plam $ \x -> plam $ \y -> plam $ \z -> plam $ \w -> f x y z w

plam5 :: (Term s a -> Term s b -> Term s c -> Term s d -> Term s e -> Term s f) -> Term s (a :--> b :--> c :--> d :--> e :--> f)
plam5 f = plam $ \x -> plam $ \y -> plam $ \z -> plam $ \w -> plam $ \w' -> f x y z w w'

plet :: Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = papp (plam f) v

pinl :: Term s a -> (Term s a -> Term s b) -> Term s b
pinl v f = f v

class PlutusType (a :: k -> Type) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> (forall b. Term s (PInner a b))
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

pcon :: PlutusType a => a s -> Term s a
pcon = punsafeCoerce . pcon'

pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x f = pmatch' (punsafeCoerce x) f

punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom = punsafeCoerce

pto :: Term s a -> (forall b. Term s (PInner a b))
pto = punsafeCoerce

data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

popaque :: Term s a -> Term s POpaque
popaque = punsafeCoerce

punsafeFromOpaque :: Term s POpaque -> Term s a
punsafeFromOpaque = punsafeCoerce

pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $ punsafeCoerce $
  plam $ \f ->
    (plam $ \(x :: Term s POpaque) -> f £ (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) £ x £ v))
    £ punsafeCoerce (plam $ \(x :: Term s POpaque) -> f £ (plam $ \(v :: Term s POpaque) -> (punsafeCoerce x) £ x £ v))
