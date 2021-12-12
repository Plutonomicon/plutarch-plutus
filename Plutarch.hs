{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch
  ( PI.Constant(..)
  , (PI.:-->)
  , PI.PDelayed
  , PI.Term
  , PI.pLam
  , PI.pApp
  , PI.pDelay
  , PI.pForce
  , PI.pHoistAcyclic
  , PI.pError
  , PI.pUnsafeCoerce
  , PI.pUnsafeBuiltin
  , PI.pUnsafeConstant
  , PI.compile
  , PI.ClosedTerm
  , PlutusType(..)
  , printTerm
  , (£$)
  , (£)
  , pLam2
  , pLam3
  , pLam4
  , pLam5
  , pLet
  , pInl
  , pCon
  , pMatch
  , pUnsafeFrom
  , pTo
) where
  
import Plutarch.Internal (Term, pApp, pUnsafeCoerce, (:-->), pLam, compile, ClosedTerm)
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
(£) = pApp
infixl 8 £

(£$) :: Term s (a :--> b) -> Term s a -> Term s b
(£$) = pApp
infixr 0 £$

-- TODO: Replace with pLamN

pLam2 :: (Term s a -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
pLam2 f = pLam $ \x -> pLam $ \y -> f x y

pLam3 :: (Term s a -> Term s b -> Term s c -> Term s d) -> Term s (a :--> b :--> c :--> d)
pLam3 f = pLam $ \x -> pLam $ \y -> pLam $ \z -> f x y z

pLam4 :: (Term s a -> Term s b -> Term s c -> Term s d -> Term s e) -> Term s (a :--> b :--> c :--> d :--> e)
pLam4 f = pLam $ \x -> pLam $ \y -> pLam $ \z -> pLam $ \w -> f x y z w

pLam5 :: (Term s a -> Term s b -> Term s c -> Term s d -> Term s e -> Term s f) -> Term s (a :--> b :--> c :--> d :--> e :--> f)
pLam5 f = pLam $ \x -> pLam $ \y -> pLam $ \z -> pLam $ \w -> pLam $ \w' -> f x y z w w'

pLet :: Term s a -> (Term s a -> Term s b) -> Term s b
pLet v f = pApp (pLam f) v

pInl :: Term s a -> (Term s a -> Term s b) -> Term s b
pInl v f = f v

class PlutusType (a :: k -> Type) where
  -- `b' :: k'` causes GHC to fail type checking at various places
  -- due to not being able to expand the type family.
  type PInner a (b' :: k -> Type) :: k -> Type
  pCon' :: forall s. a s -> (forall b. Term s (PInner a b))
  pMatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c

pCon :: PlutusType a => a s -> Term s a
pCon = pUnsafeCoerce . pCon'

pMatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pMatch x f = pMatch' (pUnsafeCoerce x) f

pUnsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
pUnsafeFrom = pUnsafeCoerce

pTo :: Term s a -> (forall b. Term s (PInner a b))
pTo = pUnsafeCoerce
