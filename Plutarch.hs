{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch (PlutusType(..), PEq(..), POrd(..), module PIC, printTerm, (£$), (£), pLam2, pLam3, pLam4, pLam5, pLet, pInl, pCon, pMatch, pUnsafeFrom, pTo) where
  
import qualified PlutusCore as PLC
import Plutarch.Internal.Core (Term, PInteger, pApp, pBuiltin, pUnsafeCoerce, pConstant, PBool, (:-->), pLam, pHoist, compile)
import Plutus.V1.Ledger.Scripts (Script(Script))
import qualified Plutarch.Internal.Core as PIC
import PlutusCore.Pretty

class PEq t where
  (£==) :: Term t -> Term t -> Term PBool

infix 4 £==

class POrd t where
  (£<=) :: Term t -> Term t -> Term PBool
  (£<) :: Term t -> Term t -> Term PBool

infix 4 £<=
infix 4 £<

instance PEq PInteger where
  x £== y = pApp (pApp (pBuiltin PLC.EqualsInteger) x) y

instance POrd PInteger where
  x £<= y = pApp (pApp (pBuiltin PLC.LessThanEqualsInteger) x) y
  x £< y = pApp (pApp (pBuiltin PLC.LessThanInteger) x) y

instance Num (Term PInteger) where
  x + y = pApp (pApp (pBuiltin PLC.AddInteger) x) y
  x - y = pApp (pApp (pBuiltin PLC.SubtractInteger) x) y
  x * y = pApp (pApp (pBuiltin PLC.MultiplyInteger) x) y
  abs x = pApp (pApp (pApp (pBuiltin PLC.IfThenElse) (x £<= (-1))) (negate x)) x -- FIXME use let
  negate x = 0 - x
  signum = undefined
  fromInteger n = pUnsafeCoerce . pConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniInteger n

-- TODO: Heavily improve. It's unreadable right now.
printTerm :: Term a -> String
printTerm term = show . prettyPlcReadableDebug . (\(Script s) -> s) . compile $ term

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

{-
_example1 :: Term (PInteger :--> PInteger)
_example1 = pLam $ \x -> x

_example2 :: Term (PInteger :--> PInteger :--> PInteger :--> PInteger)
_example2 = pLam3 $ \(x, y, z) -> x + y * z

_example3 :: Term (PInteger :--> PInteger :--> PInteger)
_example3 =
  pLam2 $ \(x, y) ->
  let z = pHoist $ _example2 £ x £ y £ y in
  z + z + 200

_example4 :: Term (PInteger :--> PInteger :--> PInteger)
_example4 =
  pLam2 $ \(x, y) ->
  pLet (_example2 £ x £ y £ y) $ \z ->
  z + z + 200
-}


{-
class FromData t where
  fromData :: Term (PData :--> t)

instance FromData PInteger where
  fromData = cBuiltin PLC.UnIData

instance FromData POpaque where
  fromData = lam $ \x -> cCoerce x

instance FromData PData where
  fromData = lam $ \x -> x

lam :: TermInterp exp => (exp a -> exp b) -> exp (a :--> b)
lam = cLam

pUnConstrData :: Term (PData :--> PPair PInteger (PList PData))
pUnConstrData = cBuiltin PLC.UnConstrData

pFst :: Term (PPair a b :--> a)
pFst = cBuiltin PLC.FstPair

pSnd :: Term (PPair a b :--> b)
pSnd = cBuiltin PLC.SndPair

pIf :: TermInterp exp => exp PBool -> exp a -> exp a -> exp a
pIf b x y = cForce $ (cBuiltin PLC.IfThenElse) £ b £ cDelay x £ cDelay y

pHead :: Term (PList a :--> a)
pHead = cBuiltin PLC.HeadList

pError :: Term a
pError = cError

pDelay :: TermInterp exp => exp a -> exp (PDelayed a)
pDelay = cDelay
pForce :: TermInterp exp => exp (PDelayed a) -> exp a
pForce = cForce

pUnsafeCoerce :: TermInterp exp => exp a -> exp b
pUnsafeCoerce = cCoerce

matchInt :: TermInterp exp => exp PInteger -> [exp a] -> exp a
matchInt x' ys' = plet x' $ \x -> go 0 x ys'
  where
    go :: TermInterp exp => Integer -> exp PInteger -> [exp a] -> exp a
    go _ _ [] = pError
    go i x (y:ys) = pIf (x £== fromInteger i) y (go (i+1) x ys)

type CurrencySymbol = POpaque
type TxOutRef = POpaque
type StakingCredential = POpaque
type DCert = POpaque

data ScriptPurpose exp where
  Minting :: exp CurrencySymbol -> ScriptPurpose exp
  Spending :: exp TxOutRef -> ScriptPurpose exp
  Rewarding :: exp StakingCredential -> ScriptPurpose exp
  Certifying :: exp DCert -> ScriptPurpose exp

instance IsPD ScriptPurpose where
  pmatch f =
    plet (pUnConstrData £ cCoerce x) $ \p ->
    plet (pFst £ p) $ \i ->
    plet (pSnd £ p) $ \l ->
    matchInt i
      [ (f . Minting $ fromData £$ pHead £ l)
      , (f . Spending $ fromData £$ pHead £ l)
      , (f . Rewarding $ fromData £$ pHead £ l)
      , (f . Certifying $ fromData £$ pHead £ l)
      ]

_example1 :: Term (PInteger :--> PInteger :--> PInteger)
_example1 = lam $ \x -> lam $ \y -> x + y + 200

_example2 :: Term (PD ScriptPurpose :--> PInteger)
_example2 = lam $ \sp -> pmatch sp $ \case
  Minting _ -> 0
  Spending _ -> 0
  Rewarding _ -> 0
  Certifying _ -> 0

{-

_example3 :: Term 'PTInteger
_example3 = _example1 £ 100 £ 100

_example4 :: Term ('PTInteger :--> 'PTInteger)
_example4 = lam $ \x -> x + 1

_example5 :: Term 'PTInteger
_example5 = _example4 £$ _example4 £$ _example4 £ 44
-}

-}
