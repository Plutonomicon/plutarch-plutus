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
  , PI.compile'
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
  
import Plutarch.Internal (Term, pApp, pUnsafeCoerce, (:-->), pLam, compile')
import qualified Plutarch.Internal as PI
import PlutusCore.Pretty
import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore.DeBruijn as PLC

-- TODO: Heavily improve. It's unreadable right now.
printTerm :: Term a -> String
printTerm term =
  show . prettyPlcReadableDebug . UPLC.termMapNames PLC.fakeNameDeBruijn . compile' $ term


(£) :: Term (a :--> b) -> Term a -> Term b
(£) = pApp
infixl 8 £

(£$) :: Term (a :--> b) -> Term a -> Term b
(£$) = pApp
infixr 0 £$

-- TODO: Replace with pLamN

pLam2 :: (Term a -> Term b -> Term c) -> Term (a :--> b :--> c)
pLam2 f = pLam $ \x -> pLam $ \y -> f x y

pLam3 :: (Term a -> Term b -> Term c -> Term d) -> Term (a :--> b :--> c :--> d)
pLam3 f = pLam $ \x -> pLam $ \y -> pLam $ \z -> f x y z

pLam4 :: (Term a -> Term b -> Term c -> Term d -> Term e) -> Term (a :--> b :--> c :--> d :--> e)
pLam4 f = pLam $ \x -> pLam $ \y -> pLam $ \z -> pLam $ \w -> f x y z w

pLam5 :: (Term a -> Term b -> Term c -> Term d -> Term e -> Term f) -> Term (a :--> b :--> c :--> d :--> e :--> f)
pLam5 f = pLam $ \x -> pLam $ \y -> pLam $ \z -> pLam $ \w -> pLam $ \w' -> f x y z w w'

pLet :: Term a -> (Term a -> Term b) -> Term b
pLet v f = pApp (pLam f) v

pInl :: Term a -> (Term a -> Term b) -> Term b
pInl v f = f v

class PlutusType a where
  type PInner a b'
  pCon' :: a -> (forall b. Term (PInner a b))
  pMatch' :: forall c. (forall b. Term (PInner a b)) -> (a -> Term c) -> Term c

pCon :: PlutusType a => a -> Term a
pCon = pUnsafeCoerce . pCon'

pMatch :: PlutusType a => Term a -> (a -> Term b) -> Term b
pMatch x f = pMatch' (pUnsafeCoerce x) f

pUnsafeFrom :: (forall b. Term (PInner a b)) -> Term a
pUnsafeFrom = pUnsafeCoerce

pTo :: Term a -> (forall b. Term (PInner a b))
pTo = pUnsafeCoerce
