{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

{- | The full definition of Plutarch 'Term's, as well as eDSL primitives which
everything else is built from.

This design follows the description of e-summaries in /Hashing Modulo
Alpha-Equivalence/. However, we also work in a monadic stack providing access
to some configuration information, as well as a source of unique identifiers
to ensure that the Barendregt convention is followed by any code generated in
Plutarch. Lastly, we provide an \'indicator\' to ensure that we never attempt
to compile terms that aren't closed, using something akin to the \'@ST@
trick\'.

= Links

- [The original paper](https://arxiv.org/pdf/2105.02856)
- [_Lazy Functional State
Threads_](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf),
which describes the \'@ST@ trick\'

@since wip
-}
module Plutarch.Backend.Term (
  TermEnv (..),
  Term (..),
  SomeTerm,
  toSomeTerm,
  TermError (..),
  PDelayed,
  plam',
  plet,
  pthrow,
  papp,
  pdelay,
  pforce,
  perror,
  pplaceholder,
  pcompiled,
  pfix,
  punsafeCoerce,
  punsafeBuiltin,
  punsafeConstant,
  punsafeConstr,
  punsafeCase,
  punsafeCompiled,
) where

import Control.Monad.Except (
  ExceptT,
  MonadError,
  runExceptT,
  throwError,
 )
import Control.Monad.RWS.CPS (
  MonadState,
  RWS,
  get,
  modify,
  runRWS,
 )
import Data.Can (Can (Eno, Non, One, Two))
import Data.Kind (Type)
import Data.Map.Merge.Strict (WhenMatched, zipWithAMatched)
import Data.Text (Text)
import Data.These (These (That, These, This))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import GHC.Stack (CallStack, HasCallStack, callStack)
import Plutarch.Backend.ANF (analyzeDemand, fromHashedAST)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.PosTree (
  PosTree (
    PCase,
    PHere,
    PMany,
    POne,
    PTwo
  ),
 )
import Plutarch.Backend.RawTerm (
  RawTerm (
    RApply,
    RBuiltin,
    RCase,
    RCompiled,
    RConstant,
    RConstr,
    RDelay,
    RError,
    RFix,
    RForce,
    RLamAbs,
    RLet,
    RPlaceholder,
    RVar
  ),
  VarTag (Argument, LetBinding, Self),
 )
import Plutarch.Backend.S (S)
import Plutarch.Backend.UPLC (UPLCTerm)
import Plutarch.Backend.VarMap (
  VarMap,
  vmDelete,
  vmEmpty,
  vmMap,
  vmMergeM,
  vmSingleton,
 )
import Plutarch.Primitive.Function ((:-->))
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

{- | A configuration environment for 'Term's and their compilation. Currently
unused.

@since wip
-}
data TermEnv = TermEnv

{- | Various errors that can arise during 'Term' construction.

@since wip
-}
data TermError
  = {- | A functional's @self@ argument went unused. This means no fixed point
    will be found, and is definitely a mistake.

    @since wip
    -}
    UnusedSelfArgument
  | {- | A general space for any user-specific errors.

    @since wip
    -}
    UserSpecified CallStack Text
  | {- | A @let@ binding construction caused a bad position tree merge. This is
    something that should not happen normally and is /definitely/ a bug!

    @since wip
    -}
    BadMergeLet Word64 PosTree PosTree
  | {- | An @apply@ construction caused a bad position tree merge. This is
    something that should not happen normally, and is /definitely/ a bug!

    @since wip
    -}
    BadMergeApply Word64 PosTree PosTree
  | {- | An @apply@ or @case@ construction caused a bad position tree merge.
    This is something that should not happen normally, and is /definitely/
    a bug!

    @since wip
    -}
    BadMergeCase Word64 PosTree PosTree
  | {- | A @constr@ construction caused a bad position tree merge. This is
    something that should not happen normally, and is /definitely/ a bug!

    @since wip
    -}
    BadMergeConstr Word64 PosTree PosTree
  deriving stock
    ( -- | @since wip
      Show
    )

{- | A basic unit of Plutarch eDSL construction. More precisely, a @'Term' s a@
is the code which corresponds to a computation that, when run, will either
produce the UPLC equivalent of @a@, or error. The @s@ is used to track free
variable dependencies, specifically to ensure that we never attempt to
compile a 'Term' with free variables.

@since wip
-}
newtype Term (s :: S) (a :: S -> Type)
  = Term {asRawTerm :: ExceptT TermError (RWS TermEnv () Word64) (VarMap, RawTerm ())}

type role Term nominal representational

{- | A 'Term' whose result has been forgotten. Useful mainly together with
'punsafeCase' and 'punsafeConstr', as it allows fields and handlers of
heterogenous types to be used in both.

@since wip
-}
data SomeTerm (s :: S) where
  SomeTerm :: Term s a -> SomeTerm s

{- | \'Forgets\' the result of a computation represented by a 'Term'.

@since wip
-}
toSomeTerm ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> SomeTerm s
toSomeTerm = SomeTerm

{- | A type-level tag indicating a Plutarch computation that has been
\'suspended\'. This tag can be removed by using 'pforce', and added by using
'pdelay'.

@since wip
-}
data PDelayed (a :: S -> Type) (s :: S)

{- | Given a code transformation from argument to result, construct the
equivalent function's code.

@since wip
-}
plam' ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () Argument)
  (vm, t) <- asRawTerm (f varTerm)
  let (mpt, vm') = vmDelete fresh vm
  pure (vmMap POne vm', RLamAbs () mpt t)

{- | Given a piece of code and a code transformation, use this to build a
@let@-binding.

= Note

This is provided purely for code clarity and better prettyprinting. The
Plutarch compiler already detects multiple-uses of the same code chunks
(modulo alpha equivalence) and performs the necessary transformations to
@let@-bind them automatically.

@since wip
-}
plet ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () LetBinding)
  (fvm, ft) <- asRawTerm (f varTerm)
  (vvm, vt) <- asRawTerm v
  let (fpt, fvm') = vmDelete fresh fvm
  let vvmExtended = vmMap (PTwo . This) vvm
  let fvmExtended = vmMap (PTwo . That) fvm'
  vm <- vmMergeM (mergeTwo BadMergeLet) vvmExtended fvmExtended
  pure (vm, RLet () fpt vt ft)

{- | Given a code transformation that takes the code for a \'self\' argument and
produces a fixpoint functional, construct the corresponding fixed point find.
This is the canonical recursion construct.

@since wip
-}
pfix ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s (a :--> b) -> Term s (a :--> b)) ->
  Term s (a :--> b)
pfix f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () Self)
  (vm, t) <- asRawTerm (f varTerm)
  let (mpt, vm') = vmDelete fresh vm
  case mpt of
    Nothing -> throwError UnusedSelfArgument
    Just pt -> pure (vmMap POne vm', RFix () pt t)

{- | Abort generating code and signal a user-specified error.

@since wip
-}
pthrow ::
  forall (a :: S -> Type) (s :: S).
  HasCallStack => Text -> Term s a
pthrow = Term . throwError . UserSpecified callStack

{- | Given the code for a function, and the code for its argument, construct the
application of that function to that argument.

@since wip
-}
papp ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) -> Term s a -> Term s b
papp f x = Term $ do
  (fvm, ft) <- asRawTerm f
  (xvm, xt) <- asRawTerm x
  let fvmExtended = vmMap (PTwo . This) fvm
  let xvmExtended = vmMap (PTwo . That) xvm
  merged <- vmMergeM (mergeTwo BadMergeApply) fvmExtended xvmExtended
  pure (merged, RApply () ft xt)

{- | Given the code for @a@, construct code that delays its evaluation.

@since wip
-}
pdelay ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Term s (PDelayed a)
pdelay t = Term $ do
  (vm, t') <- asRawTerm t
  pure (vmMap POne vm, RDelay () t')

{- | Given a piece of code producing a delayed result, construct code forcing
its evaluation.

@since wip
-}
pforce ::
  forall (a :: S -> Type) (s :: S).
  Term s (PDelayed a) -> Term s a
pforce t = Term $ do
  (vm, t') <- asRawTerm t
  pure (vmMap POne vm, RForce () t')

{- | Construct the canonical UPLC error.

@since wip
-}
perror :: forall (a :: S -> Type) (s :: S). Term s a
perror = Term . pure $ (vmEmpty, RError ())

{- | Constructs a placeholder term. It is extremely unlikely that you need to
use this: it is needed only to compile certain record-related code.
Furthermore, any placeholders remaining in code when compiling are treated as
errors.

@since wip
-}
pplaceholder ::
  forall (a :: S -> Type) (s :: S).
  Integer -> Term s a
pplaceholder i = Term . pure $ (vmEmpty, RPlaceholder () i)

{- | \'Retag\' a 'Term' to specify it as producing a different result. This is
extremely unsafe, as it can subvert any and all type system guarantees of
Plutarch. Use with care.

@since wip
-}
punsafeCoerce ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> Term s b
punsafeCoerce = Term . asRawTerm

{- | Produce code corresponding to the stated builtin. This is marked as unsafe,
as polymorphic builtins may require their arguments to be presented in
special ways, or require some number of 'pforce's to be added to make sure
they are called correctly. If you want to use this directly, ensure you
verify this is being done.

@since wip
-}
punsafeBuiltin ::
  forall (a :: S -> Type) (s :: S).
  PLC.DefaultFun -> Term s a
punsafeBuiltin f = Term . pure $ (vmEmpty, RBuiltin () f)

{- | Given some element of the Plutus default universe, produce code that yields
it, marked by an equivalent type @a@. This is marked as unsafe as there are
no guarantees that @a@ corresponds appropriately. Ensure this holds if you
plan to use this directly.

@since wip
-}
punsafeConstant ::
  forall (a :: S -> Type) (s :: S).
  Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstant c = Term . pure $ (vmEmpty, RConstant () c)

{- | Given a tag, and an existentially-erased 'Vector' of fields, construct a
@constr@ term producing an SOP with the given tag and the stated fields.

= Note

The existential erasure allows the use of fields that don't have identical
types in Plutarch. This can be quite unsafe, as there is no guarantee that
you get the result you are after: ensure that the types make sense for the
SOP you're trying to construct.

@since wip
-}
punsafeConstr ::
  forall (a :: S -> Type) (s :: S).
  Word64 ->
  Vector (SomeTerm s) ->
  Term s a
punsafeConstr ix fields = Term $ do
  -- Note (Koz, 28/05/2026): We need to use the constructor explicitly here, as
  -- `asRawTerm` can't solve for the existential for some reason.
  fields' <- traverse (\(SomeTerm (Term t)) -> t) fields
  let len = Vector.length fields
  vm <- Vector.ifoldM (go len) vmEmpty . fmap fst $ fields'
  pure (vm, RConstr () ix . fmap snd $ fields')
  where
    go ::
      forall (m :: Type -> Type).
      MonadError TermError m =>
      Int -> VarMap -> Int -> VarMap -> m VarMap
    go len acc ix = vmMergeM mergeConstr acc . vmMap (toConstr len ix)

{- | Given the code of a scrutinee, and an existentially-erased 'NonEmptyVector'
of code for handlers, construct a @case@ statement over said scrutinee, using
the handlers specified in order.

= Note

The existential erasure allows the use of handlers that don't have identical
types in Plutarch. This is extremely unsafe, as there are no guarantees that
your handlers are of the correct form, provided in the correct order, or even
that the scrutinee can be @case@d on at all. Ensure you verify all this when
using.

@since wip
-}
punsafeCase ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a ->
  NonEmptyVector (SomeTerm s) ->
  Term s b
punsafeCase scrut handlers = Term $ do
  (vmScrut, tscrut) <- asRawTerm scrut
  -- Note (Koz, 28/05/2026): We need to use the constructor explicitly here, as
  -- `asRawTerm` can't solve for the existential for some reason.
  handlers' <- traverse (\(SomeTerm (Term t)) -> t) handlers
  let len = NEVector.length handlers
  let vmScrutExtended = vmMap (\pt -> PCase (Just pt) . NEVector.replicate1 len $ Nothing) vmScrut
  vm <- NEVector.ifoldM (go len) vmScrutExtended . fmap fst $ handlers'
  pure (vm, RCase () tscrut . fmap snd $ handlers')
  where
    go ::
      forall (m :: Type -> Type).
      MonadError TermError m => Int -> VarMap -> Int -> VarMap -> m VarMap
    go len acc ix = vmMergeM mergeCase acc . vmMap (toCase len ix)

{- | Given a closed 'Term', compile it \'on the spot\', and embed the resulting
UPLC into a 'Term'.

= Note

This is rarely useful or efficient. Plutarch's code generator treats any such
\'code blobs\' as essentially opaque, and can only perform those
optimizations that are possible at the time 'pcompiled' is used. More
precisely, any 'Term' that uses the result of a 'pcompiled' will not be able
to \'see inside\' that result, inhibiting any kind of global analysis.

Only use this if you are certain this gives you any benefit. The primary use
case for this is property-based testing.

@since wip
-}
pcompiled ::
  forall (a :: S -> Type) (s :: S).
  (forall (s' :: S). Term s' a) ->
  Term s a
pcompiled (Term t) = case runRWS (runExceptT t) TermEnv 0 of
  (res, _, _) -> case res of
    Left err -> Term . throwError $ err
    -- We know that we have a closed term, so we can ignore the varmap.
    Right (_, rt) ->
      let ast = fromRawTerm rt
          anf = fromHashedAST ast
          analyzedANF = analyzeDemand anf
       in Term . pure $ (vmEmpty, RCompiled () . toUPLCTerm $ analyzedANF)

{- | As 'pcompiled', but uses a 'UPLCTerm' directly. The 'UPLCTerm' is assumed
to be closed, but this won't be checked.

= Note

In addition to all the caveats of 'pcompiled', 'punsafeCompiled' is
/extremely/ unsafe. There is no guarantee that the correct type of the
'UPLCTerm' \'blob\' provided matches what you request, that the 'UPLCTerm' is
closed, or even that it is sensible. Furthermore, Plutarch is unable to
optimize this /at all/, even locally. Be /very/ sure this is worthwhile
before you attempt it!

@since wip
-}
punsafeCompiled ::
  forall (a :: S -> Type) (s :: S).
  UPLCTerm ->
  Term s a
punsafeCompiled t = Term . pure $ (vmEmpty, RCompiled () t)

-- Helpers

-- Helper for extending position trees for `case`s
toCase :: Int -> Int -> PosTree -> PosTree
toCase len ix pt = PCase Nothing . NEVector.generate1 len $ \ix' -> if ix == ix' then Just pt else Nothing

-- Helper for extending position trees for `constr`s
toConstr :: Int -> Int -> PosTree -> PosTree
toConstr len ix pt = PMany . Vector.generate len $ \ix' -> if ix == ix' then Just pt else Nothing

-- Handler for combining `constr` position trees
mergeConstr ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  WhenMatched m Word64 PosTree PosTree PosTree
mergeConstr = zipWithAMatched $ \k v1 v2 -> case v1 of
  PMany xs -> case v2 of
    PMany ys -> do
      let combined = Vector.zipWith maybeToCan xs ys
      PMany <$> traverse (mergeCanM (BadMergeConstr k v1 v2)) combined
    _ -> throwError . BadMergeConstr k v1 $ v2
  _ -> throwError . BadMergeConstr k v1 $ v2

-- Handler for combining `PTwo`s, needed in various places
mergeTwo ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  (Word64 -> PosTree -> PosTree -> TermError) ->
  WhenMatched m Word64 PosTree PosTree PosTree
mergeTwo mkErr = zipWithAMatched $ \k v1 v2 -> case v1 of
  PTwo (This t1) -> case v2 of
    PTwo (That t2) -> pure . PTwo . These t1 $ t2
    _ -> throwError . mkErr k v1 $ v2
  _ -> throwError . mkErr k v1 $ v2

-- Handler for combining position trees for `case`s
mergeCase ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  WhenMatched m Word64 PosTree PosTree PosTree
mergeCase = zipWithAMatched $ \k v1 v2 -> case v1 of
  PCase func1 args1 -> case v2 of
    PCase func2 args2 -> do
      let funcs = maybeToCan func1 func2
      let args = NEVector.zipWith maybeToCan args1 args2
      func <- mergeCanM (BadMergeCase k v1 v2) funcs
      args' <- traverse (mergeCanM (BadMergeCase k v1 v2)) args
      pure . PCase func $ args'
    _ -> throwError . BadMergeCase k v1 $ v2
  _ -> throwError . BadMergeCase k v1 $ v2

freshAndIncrement ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadState a m, Num a) =>
  m a
freshAndIncrement = do
  fresh <- get
  modify (+ 1)
  pure fresh

maybeToCan ::
  forall (a :: Type) (b :: Type).
  Maybe a -> Maybe b -> Can a b
maybeToCan x y = case x of
  Nothing -> maybe Non Eno y
  Just x' -> case y of
    Nothing -> One x'
    Just y' -> Two x' y'

mergeCanM ::
  forall (e :: Type) (m :: Type -> Type) (a :: Type).
  MonadError e m =>
  e ->
  Can a a ->
  m (Maybe a)
mergeCanM err = \case
  Non -> pure Nothing
  One x -> pure . Just $ x
  Eno x -> pure . Just $ x
  Two _ _ -> throwError err
