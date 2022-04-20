{-# LANGUAGE DefaultSignatures #-}

module Plutarch.Rec (
  DataReader (DataReader, readData),
  DataWriter (DataWriter, writeData),
  PRecord (PRecord, getRecord),
  ScottEncoded,
  ScottEncoding,
  RecordFromData (fieldFoci, fieldListFoci),
  field,
  fieldFromData,
  letrec,
  pletrec,
  rcon,
  recordDataFromFieldWriters,
  recordFromFieldReaders,
  rmatch,
) where

import Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Kind (Type)
import Data.Monoid (Dual (Dual, getDual), Endo (Endo, appEndo), Sum (Sum, getSum))
import GHC.Word (Word64)
import Plutarch (
  PlutusType (PInner, pcon', pmatch'),
  pcon,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  (#),
  (:-->),
 )
import Plutarch.Bool (pif, (#==))
import Plutarch.Builtin (PAsData, PBuiltinList, PData, pasConstr, pforgetData, pfstBuiltin, psndBuiltin)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (
  PType,
  RawTerm (RApply, RLamAbs, RVar),
  Term (Term, asRawTerm),
  TermResult (TermResult, getDeps, getTerm),
  mapTerm,
 )
import Plutarch.List (pcons, phead, pnil, ptail)
import Plutarch.Trace (ptraceError)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import qualified PlutusCore as PLC
import qualified Rank2

newtype PRecord r s = PRecord {getRecord :: r (Term s)}

type family ScottEncoded (r :: ((PType) -> Type) -> Type) (a :: PType) :: PType
newtype ScottArgument r s t = ScottArgument {getScott :: Term s (ScottEncoded r t)}
type ScottEncoding r t = ScottEncoded r t :--> t

instance (Rank2.Distributive r, Rank2.Traversable r) => PlutusType (PRecord r) where
  type PInner (PRecord r) t = ScottEncoding r t
  pcon' :: forall s t. PRecord r s -> Term s (ScottEncoding r t)
  pcon' (PRecord r) = rcon r
  pmatch' :: forall s t. (Term s (ScottEncoding r t)) -> (PRecord r s -> Term s t) -> Term s t
  pmatch' p f = rmatch p (f . PRecord)

-- | Convert a Haskell record value to a Scott-encoded record.
rcon :: forall r s t. Rank2.Foldable r => r (Term s) -> Term s (ScottEncoding r t)
rcon r = plam (\f -> punsafeCoerce $ appEndo (getDual $ Rank2.foldMap (Dual . Endo . applyField) r) f)
  where
    applyField x f = punsafeCoerce f # x

-- | Match a Scott-encoded record using a function that takes a Haskell record value.
rmatch ::
  forall r s t.
  (Rank2.Distributive r, Rank2.Traversable r) =>
  (Term s (ScottEncoding r t)) ->
  (r (Term s) -> Term s t) ->
  Term s t
rmatch p f = p # arg
  where
    arg :: Term s (ScottEncoded r t)
    arg = Term (\i -> TermResult (RLamAbs (fieldCount (initial @r) - 1) $ rawArg i) [])
    rawArg :: Word64 -> RawTerm
    rawArg depth = getTerm $ asRawTerm (f $ variables depth) $ depth + fieldCount (initial @r)

-- | Wrapped recursive let construct, tying into knot the recursive equations specified in the record fields.
pletrec :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => (r (Term s) -> r (Term s)) -> Term s (PRecord r)
pletrec = punsafeCoerce . letrec

-- | Recursive let construct, tying into knot the recursive equations specified in the record fields.
letrec ::
  forall r s t.
  (Rank2.Distributive r, Rank2.Traversable r) =>
  (r (Term s) -> r (Term s)) ->
  Term s (ScottEncoding r t)
letrec r = Term term
  where
    term n = TermResult {getTerm = RApply rfix [RLamAbs 1 $ RApply (RVar 0) $ rawTerms], getDeps = deps}
      where
        (Dual rawTerms, deps) = Rank2.foldMap (rawResult . ($ n + 2) . asRawTerm) (r selfReferring)
        selfReferring = Rank2.fmap fromRecord accessors
        fromRecord :: ScottArgument r s a -> Term s a
        fromRecord (ScottArgument (Term access)) =
          Term $ \depth -> mapTerm (\field -> RApply (RVar $ depth - n - 1) [field]) (access 0)
    rawResult TermResult {getTerm, getDeps} = (Dual [getTerm], getDeps)

-- | Converts a Haskell field function to a Scott-encoded record field accessor.
field ::
  forall r s t.
  (Rank2.Distributive r, Rank2.Traversable r) =>
  (r (ScottArgument r s) -> ScottArgument r s t) ->
  Term s (ScottEncoded r t)
field f = getScott (f accessors)

-- | Provides a record of function terms that access each field out of a Scott-encoded record.
accessors :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => r (ScottArgument r s)
accessors = abstract Rank2.<$> variables 0
  where
    abstract :: Term s a -> ScottArgument r s a
    abstract (Term t) = ScottArgument (phoistAcyclic $ Term $ mapTerm (RLamAbs $ depth - 1) . t . (depth +))
    depth = fieldCount (initial @r)

{- | A record of terms that each accesses a different variable in scope,
 outside in following the field order.
-}
variables :: forall r s. (Rank2.Distributive r, Rank2.Traversable r) => Word64 -> r (Term s)
variables baseDepth = Rank2.cotraverse var id
  where
    var :: (r (Term s) -> Term s a) -> Term s a
    var ref = ref ordered
    ordered :: r (Term s)
    ordered = evalState (Rank2.traverse next $ initial @r) 0
    next :: f a -> State Word64 (Term s a)
    next _ = do
      i <- get
      let i' = succ i
      seq i' (put i')
      return $
        Term $
          \depth ->
            TermResult
              { getTerm = RVar (depth - baseDepth - i')
              , getDeps = []
              }

newtype DataReader s a = DataReader {readData :: Term s (PAsData a) -> Term s a}
newtype DataWriter s a = DataWriter {writeData :: Term s a -> Term s (PAsData a)}
newtype FocusFromData s a b = FocusFromData {getFocus :: Term s (PAsData a :--> PAsData b)}
newtype FocusFromDataList s a = FocusFromDataList {getItem :: Term s (PBuiltinList PData) -> Term s (PAsData a)}

{- | Converts a record of field DataReaders to a DataReader of the whole
 record. If you only need a single field or two, use `fieldFromData`
 instead.
-}
recordFromFieldReaders ::
  forall r s.
  (Rank2.Apply r, RecordFromData r) =>
  r (DataReader s) ->
  DataReader s (PRecord r)
recordFromFieldReaders reader = DataReader $ verifySoleConstructor readRecord
  where
    readRecord :: Term s (PBuiltinList PData) -> Term s (PRecord r)
    readRecord dat = pcon $ PRecord $ Rank2.liftA2 (flip readData . getCompose) (fields dat) reader
    fields :: Term s (PBuiltinList PData) -> r (Compose (Term s) PAsData)
    fields bis = (\f -> Compose $ getItem f bis) Rank2.<$> fieldListFoci

recordDataFromFieldWriters ::
  forall r s.
  (Rank2.Apply r, RecordFromData r) =>
  r (DataWriter s) ->
  DataWriter s (PRecord r)
recordDataFromFieldWriters writer = DataWriter (`pmatch` writeRecord)
  where
    writeRecord :: PRecord r s -> Term s (PAsData (PRecord r))
    writeRecord (PRecord r) =
      punsafeBuiltin PLC.ConstrData # (0 :: Term s PInteger)
        # appEndo (Rank2.foldMap (Endo . consField) (Rank2.liftA2 writeField writer r)) pnil
    consField :: Compose (Term s) PAsData a -> Term s (PBuiltinList PData) -> Term s (PBuiltinList PData)
    consField (Compose h) t = pcons # pforgetData h # t
    writeField :: DataWriter s a -> Term s a -> Compose (Term s) PAsData a
    writeField w r = Compose (writeData w r)

{- | Converts a Haskell field function to a function term that extracts the 'Data' encoding of the field from the
 encoding of the whole record. If you need to access most of the record fields, it's more efficient to decode the
 entire record at once with `recordFromFieldReaders`.
-}
fieldFromData ::
  RecordFromData r =>
  (r (FocusFromData s (PRecord r)) -> FocusFromData s (PRecord r) t) ->
  Term s (PAsData (PRecord r) :--> PAsData t)
fieldFromData f = getFocus (f fieldFoci)

{- | Instances of this class must know how to focus on individual fields of
 the data-encoded record. If the declared order of the record fields doesn't
 match the encoding order, you must override the method defaults.
-}
class (Rank2.Distributive r, Rank2.Traversable r) => RecordFromData r where
  -- | Given the encoding of the whole record, every field focuses on its own encoding.
  fieldFoci :: r (FocusFromData s (PRecord r))

  -- | Given the encoding of the list of all fields, every field focuses on its own encoding.
  fieldListFoci :: r (FocusFromDataList s)

  fieldFoci = Rank2.cotraverse focus id
    where
      focus :: (r (FocusFromData s (PRecord r)) -> FocusFromData s (PRecord r) a) -> FocusFromData s (PRecord r) a
      focus ref = ref foci
      foci :: r (FocusFromData s (PRecord r))
      foci = fieldsFromRecord Rank2.<$> fieldListFoci
      fieldsFromRecord :: FocusFromDataList s a -> FocusFromData s (PRecord r) a
      fieldsFromRecord (FocusFromDataList f) = FocusFromData $ plam $ verifySoleConstructor f
  fieldListFoci = Rank2.cotraverse focus id
    where
      focus :: (r (FocusFromDataList s) -> FocusFromDataList s a) -> FocusFromDataList s a
      focus ref = ref foci
      foci :: r (FocusFromDataList s)
      foci = evalState (Rank2.traverse next $ initial @r) id
      next :: f a -> State (Term s (PBuiltinList PData) -> Term s (PBuiltinList PData)) (FocusFromDataList s a)
      next _ = do
        rest <- get
        put ((ptail #) . rest)
        return $ FocusFromDataList (punsafeCoerce . (phead #) . rest)

verifySoleConstructor :: (Term s (PBuiltinList PData) -> Term s a) -> (Term s (PAsData (PRecord r)) -> Term s a)
verifySoleConstructor f d =
  plet (pasConstr # pforgetData d) $ \constr ->
    pif
      (pfstBuiltin # constr #== 0)
      (f $ psndBuiltin # constr)
      (ptraceError "verifySoleConstructor failed")

initial :: Rank2.Distributive r => r (Compose Maybe (Term s))
initial = Rank2.distribute Nothing

fieldCount :: Rank2.Foldable r => r f -> Word64
fieldCount = getSum . Rank2.foldMap (const $ Sum 1)

-- | The raw Y-combinator term
rfix :: RawTerm
-- The simplest variant of the Y combinator hangs the interpreter, so we use an eta-expanded version instead.
-- rfix = RLamAbs 0 $ RApply (RLamAbs 0 $ RApply (RVar 1) [RApply (RVar 0) [RVar 0]]) [RLamAbs 0 $ RApply (RVar 1) [RApply (RVar 0) [RVar 0]]]
rfix =
  RLamAbs 0 $
    RApply
      (RLamAbs 0 $ RApply (RVar 1) [RLamAbs 0 $ RApply (RVar 1) [RVar 0, RVar 1]])
      [RLamAbs 0 $ RApply (RVar 1) [RLamAbs 0 $ RApply (RVar 1) [RVar 0, RVar 1]]]
