module Plutarch.Rec (
  PRecord (PRecord, getRecord),
  ScottEncoded,
  ScottEncoding,
  field,
  letrec,
  pletrec,
) where

import Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import Data.Functor.Compose (Compose)
import Data.Kind (Type)
import Data.Monoid (Dual (Dual, getDual), Endo (Endo, appEndo), Sum (Sum, getSum))
import Numeric.Natural (Natural)
import Plutarch (PlutusType (PInner, pcon', pmatch'), phoistAcyclic, plam, punsafeCoerce, (#), (:-->))
import Plutarch.Internal (
  PType,
  RawTerm (RApply, RLamAbs, RVar),
  Term (Term, asRawTerm),
  TermResult (TermResult, getDeps, getTerm),
  mapTerm,
 )
import qualified Rank2

newtype PRecord r s = PRecord {getRecord :: r (Term s)}

type family ScottEncoded (r :: ((PType) -> Type) -> Type) (a :: PType) :: PType
newtype ScottArgument r s t = ScottArgument {getScott :: Term s (ScottEncoded r t)}
type ScottEncoding r t = ScottEncoded r t :--> t

instance (Rank2.Distributive r, Rank2.Traversable r) => PlutusType (PRecord r) where
  type PInner (PRecord r) t = ScottEncoding r t
  pcon' :: forall s t. PRecord r s -> Term s (ScottEncoding r t)
  pcon' = rcon . getRecord
  pmatch' :: forall s t. (forall t. Term s (ScottEncoding r t)) -> (PRecord r s -> Term s t) -> Term s t
  pmatch' p f = p # arg
    where
      arg :: Term s (ScottEncoded r t)
      arg = Term (\i -> TermResult (RLamAbs (fieldCount (initial @r) - 1) $ rawArg i) [])
      rawArg :: Natural -> RawTerm
      rawArg depth = getTerm $ asRawTerm (f $ PRecord variables) $ depth + fieldCount (initial @r)

rcon :: forall r s t. Rank2.Foldable r => r (Term s) -> Term s (ScottEncoding r t)
rcon r = plam (\f -> punsafeCoerce $ appEndo (getDual $ Rank2.foldMap (Dual . Endo . applyField) r) f)
  where
    applyField x f = punsafeCoerce f # x

-- | Wrapped recursive let construct, tying into knot the recursive equations specified in the record fields.
pletrec :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => (r (Term s) -> r (Term s)) -> Term s (PRecord r)
pletrec = punsafeCoerce . letrec

-- | Recursive let construct, tying into knot the recursive equations specified in the record fields.
letrec :: forall r s t. (Rank2.Distributive r, Rank2.Traversable r) => (r (Term s) -> r (Term s)) -> Term s (ScottEncoding r t)
letrec r = Term term
  where
    term n = TermResult {getTerm = RApply rfix [RLamAbs 1 $ RApply (RVar 0) $ rawTerms], getDeps = deps}
      where
        (Dual rawTerms, deps) = Rank2.foldMap (rawResult . ($ n) . asRawTerm) (r selfReferring)
    rawResult TermResult {getTerm, getDeps} = (Dual [getTerm], getDeps)
    selfReferring = Rank2.fmap fromRecord accessors
    fromRecord :: ScottArgument r s a -> Term s a
    fromRecord (ScottArgument (Term access)) =
      Term $ \depth -> mapTerm (\field -> RApply (RVar $ fieldCount (initial @r) + depth - 1) [field]) (access 0)

-- | Converts a Haskell field function to a Scott-encoded record field accessor.
field ::
  forall r s t.
  (Rank2.Distributive r, Rank2.Traversable r) =>
  (r (ScottArgument r s) -> ScottArgument r s t) ->
  Term s (ScottEncoded r t)
field f = getScott (f accessors)

-- | Provides a record of function terms that access each field out of a Scott-encoded record.
accessors :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => r (ScottArgument r s)
accessors = abstract Rank2.<$> variables
  where
    abstract :: Term s a -> ScottArgument r s a
    abstract (Term t) = ScottArgument (phoistAcyclic $ Term $ mapTerm (RLamAbs $ fieldCount (initial @r) - 1) . t)

{- | A record of terms that each accesses a different variable in scope,
 outside in following the field order.
-}
variables :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => r (Term s)
variables = Rank2.cotraverse var id
  where
    var :: (r (Term s) -> Term s a) -> Term s a
    var ref = ref ordered
    ordered :: r (Term s)
    ordered = evalState (Rank2.traverse next $ initial @r) (fieldCount $ initial @r)
    next :: f a -> State Natural (Term s a)
    next _ = do
      i <- get
      let i' = pred i
      seq i' (put i')
      return $
        Term $
          const $
            TermResult
              { getTerm = RVar i'
              , getDeps = []
              }

initial :: Rank2.Distributive r => r (Compose Maybe (Term s))
initial = Rank2.distribute Nothing

fieldCount :: Rank2.Foldable r => r f -> Natural
fieldCount = getSum . Rank2.foldMap (const $ Sum 1)

-- | The raw Y-combinator term
rfix :: RawTerm
-- The simplest variant of the Y combinator hangs the interpreter, so we use an eta-expanded version instead.
-- rfix = RLamAbs 0 $ RApply (RLamAbs 0 $ RApply (RVar 1) [RApply (RVar 0) [RVar 0]]) [RLamAbs 0 $ RApply (RVar 1) [RApply (RVar 0) [RVar 0]]]
rfix = RLamAbs 0 $ RApply (RLamAbs 0 $ RApply (RVar 1) [RLamAbs 0 $ RApply (RVar 1) [RVar 0, RVar 1]]) [RLamAbs 0 $ RApply (RVar 1) [RLamAbs 0 $ RApply (RVar 1) [RVar 0, RVar 1]]]
