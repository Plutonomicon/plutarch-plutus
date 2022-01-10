module Plutarch.Rec (
  PRecord (PRecord, getRecord),
  ScottArgument,
  ScottEncoded,
  ScottEncoding,
  accessors,
  letrec,
  (#.),
) where

import Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import Data.Functor.Compose (Compose)
import Data.Kind (Type)
import Data.Monoid (Dual (Dual, getDual), Endo (Endo, appEndo), Sum (Sum, getSum))
import Numeric.Natural (Natural)
import Plutarch (PCon, pcon, phoistAcyclic, plam, punsafeCoerce, (#), (:-->))
import Plutarch.Internal (
  RawTerm (RApply, RLamAbs, RVar),
  Term (Term, asRawTerm),
  TermResult (TermResult, getDeps, getTerm),
  mapTerm)
import qualified Rank2

{- |
  Highest precedence infixl operator, to be used like record field accessor. e.g.:

  >>> record #. field
-}
(#.) ::
  forall r s t.
  (Rank2.Distributive r, Rank2.Traversable r) =>
  Term s (PRecord r) ->
  (r (ScottArgument r s) -> ScottArgument r s t) ->
  Term s t
r #. f = (punsafeCoerce r :: Term s (ScottEncoding r t)) # getScott (f accessors)

infixl 9 #.

newtype PRecord r s = PRecord {getRecord :: r (Term s)}

type family ScottEncoded (r :: ((k -> Type) -> Type) -> Type) (a :: k -> Type) :: k -> Type
newtype ScottArgument r s t = ScottArgument {getScott :: Term s (ScottEncoded r t)}
type ScottEncoding r t = ScottEncoded r t :--> t

instance {-# OVERLAPS #-} Rank2.Foldable r => PCon (PRecord r) where
  pcon :: forall s. PRecord r s -> Term s (PRecord r)
  --  pcon (PRecord (SampleRecord b i s)) = punsafeCoerce (plam (\f-> f # b # i # s :: Term s a))
  pcon = punsafeCoerce . rcon . getRecord

rcon :: forall r s t. Rank2.Foldable r => r (Term s) -> Term s (ScottEncoding r t)
rcon r = plam (\f -> punsafeCoerce $ appEndo (getDual $ Rank2.foldMap (Dual . Endo . applyField) r) f)
  where
    applyField x f = punsafeCoerce f # x

-- | Recursive let construct, tying into knot the recursive equations specified in the record fields.
letrec :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => (r (Term s) -> r (Term s)) -> Term s (PRecord r)
letrec = punsafeCoerce . letrec'

letrec' :: forall r s t. (Rank2.Distributive r, Rank2.Traversable r) => (r (Term s) -> r (Term s)) -> Term s (ScottEncoding r t)
letrec' r = Term term
  where
    term n = TermResult {getTerm = RApply rfix [RLamAbs 1 $ RApply (RVar 0) $ rawTerms], getDeps = deps}
      where
        (Dual rawTerms, deps) = Rank2.foldMap (rawResult . ($ n) . asRawTerm) (r selfReferring)
    rawResult TermResult {getTerm, getDeps} = (Dual [getTerm], getDeps)
    selfReferring = Rank2.fmap fromRecord accessors
    fromRecord (ScottArgument (Term access)) = Term $ \depth -> mapTerm (\field -> RApply (RVar $ fieldCount + depth - 1) [field]) (access 0)
    fieldCount :: Natural
    fieldCount = getSum (Rank2.foldMap (const $ Sum 1) (accessors @r))

-- | Provides a record of function terms that access each field out of a Scott-encoded record.
accessors :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => r (ScottArgument r s)
accessors = Rank2.cotraverse accessor id
  where
    accessor :: (r (ScottArgument r s) -> ScottArgument r s a) -> ScottArgument r s a
    accessor ref = ref ordered
    ordered :: r (ScottArgument r s)
    ordered = evalState (Rank2.traverse next initial) fieldCount
    initial :: r (Compose Maybe (ScottArgument r s))
    initial = Rank2.distribute Nothing
    next :: f a -> State Natural (ScottArgument r s a)
    next _ = do
      i <- get
      let i' = pred i
      seq i' (put i')
      return
        ( ScottArgument $
            phoistAcyclic $
              Term $
                const $
                  TermResult
                    { getTerm = RLamAbs (fieldCount - 1) $ RVar i'
                    , getDeps = []
                    }
        )
    fieldCount :: Natural
    fieldCount = getSum (Rank2.foldMap (const $ Sum 1) initial)

-- | The raw Y-combinator term
rfix :: RawTerm
-- The simplest variant of the Y combinator hangs the interpreter, so we use an eta-expanded version instead.
-- rfix = RLamAbs 0 $ RApply (RLamAbs 0 $ RApply (RVar 1) [RApply (RVar 0) [RVar 0]]) [RLamAbs 0 $ RApply (RVar 1) [RApply (RVar 0) [RVar 0]]]
rfix = RLamAbs 0 $ RApply (RLamAbs 0 $ RApply (RVar 1) [RLamAbs 0 $ RApply (RVar 1) [RVar 0, RVar 1]]) [RLamAbs 0 $ RApply (RVar 1) [RLamAbs 0 $ RApply (RVar 1) [RVar 0, RVar 1]]]
