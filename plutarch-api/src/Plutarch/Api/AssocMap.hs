{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module is designed to be imported qualified, as many of its
identifiers clash with the Plutarch prelude.
-}
module Plutarch.Api.AssocMap (
  -- * Types
  PMap (..),
  KeyGuarantees (..),
  Commutativity (..),

  -- * Functions

  -- ** Creation
  pempty,

  -- ** Transformation
  passertSorted,
  pmap,
  pmapData,
  pmapMaybe,
  pmapMaybeData,

  -- ** Relational lift
  pcheckBinRel,

  -- ** Folds
  pall,

  -- ** Combination
  punionResolvingCollisionsWith,

  -- ** Queries
  pnull,
) where

import Data.Bifunctor (bimap)
import Data.Proxy (Proxy (Proxy))
import Data.Traversable (for)
import Plutarch.Api.Utils (Mret)
import Plutarch.Bool (PSBool (PSFalse, PSTrue), psfalse, pstrue)
import Plutarch.Builtin (
  pasMap,
  pdataImpl,
  pforgetData,
  pfromDataImpl,
  ppairDataBuiltin,
 )
import Plutarch.Internal (punsafeBuiltin)
import Plutarch.Internal.Witness (witness)
import Plutarch.Lift (
  PConstantDecl,
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.List qualified as List
import Plutarch.Prelude hiding (pall, pmap, pnull, pzipWith)
import Plutarch.Prelude qualified as PPrelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.AssocMap qualified as PlutusMap
import Prelude hiding (pred)

-- TODO: Rename this, because this is actually a _sorting_ guarantee!

-- | @since 2.0.0
data KeyGuarantees = Sorted | Unsorted

-- | @since 2.0.0
newtype PMap (keysort :: KeyGuarantees) (k :: PType) (v :: PType) (s :: S)
  = PMap (Term s (PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PMap keysort k v) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PIsData (PMap keysort k v) where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

-- | @since 2.0.0
instance PEq (PMap 'Sorted k v) where
  x #== y = peqViaData # x # y
    where
      peqViaData ::
        forall (s :: S).
        Term s (PMap 'Sorted k v :--> PMap 'Sorted k v :--> PBool)
      peqViaData = phoistAcyclic $ plam $ \m0 m1 -> pdata m0 #== pdata m1

-- | @since 2.0.0
instance
  ( PLiftData k
  , PLiftData v
  , Ord (PLifted k)
  ) =>
  PUnsafeLiftDecl (PMap 'Unsorted k v)
  where
  type PLifted (PMap 'Unsorted k v) = PlutusMap.Map (PLifted k) (PLifted v)

-- | @since 2.0.0
instance
  ( PConstantData k
  , PConstantData v
  , Ord k
  ) =>
  PConstantDecl (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap 'Unsorted (PConstanted k) (PConstanted v)
  pconstantToRepr m = bimap Plutus.toData Plutus.toData <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    for m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

-- | @since 2.0.0
instance
  ( PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Unsorted k v))
  where
  type PTryFromExcess PData (PAsData (PMap 'Unsorted k v)) = Mret (PMap 'Unsorted k v)
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasMap # opq
    unwrapped <- tcont . plet $ List.pmap # ptryFromPair # opq'
    pure (punsafeCoerce opq, pcon . PMap $ unwrapped)
    where
      ptryFromPair ::
        forall (s :: S).
        Term s (PBuiltinPair PData PData :--> PBuiltinPair (PAsData k) (PAsData v))
      ptryFromPair = plam $ \p ->
        ppairDataBuiltin
          # ptryFrom (pfstBuiltin # p) fst
          # ptryFrom (psndBuiltin # p) fst

-- | @since 2.0.0
instance
  ( POrd k
  , PIsData k
  , PIsData v
  , PTryFrom PData (PAsData k)
  , PTryFrom PData (PAsData v)
  ) =>
  PTryFrom PData (PAsData (PMap 'Sorted k v))
  where
  type PTryFromExcess PData (PAsData (PMap 'Sorted k v)) = Mret (PMap 'Sorted k v)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PMap 'Unsorted k v)) opq
    unwrapped <- tcont $ plet . papp passertSorted . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since 2.0.0
data Commutativity = Commutative | NonCommutative
  deriving stock
    ( -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      Ord
    , -- | @since 2.0.0
      Show
    )

-- TODO: Rename this, because the name is confusing.

{- | Given a 'PMap' of uncertain order, yield a 'PMap' that is known to be
sorted.

@since 2.0.0
-}
passertSorted ::
  forall (k :: S -> Type) (v :: S -> Type) (any :: KeyGuarantees) (s :: S).
  (POrd k, PIsData k, PIsData v) =>
  Term s (PMap any k v :--> PMap 'Sorted k v)
passertSorted =
  let _ = witness (Proxy :: Proxy (PIsData v))
   in phoistAcyclic $
        plam $ \m ->
          precList
            ( \self x xs ->
                plet (pfromData $ pfstBuiltin # x) $ \k ->
                  plam $ \badKey ->
                    pif
                      (badKey # k)
                      (ptraceError "unsorted map")
                      (self # xs # plam (#< k))
            )
            -- this is actually the empty map so we can
            -- safely assume that it is sorted
            (const . plam . const $ punsafeCoerce m)
            # pto m
            # plam (const $ pcon PFalse)

{- | Construct an empty 'PMap'.

@since 2.0.0
-}
pempty :: Term s (PMap 'Sorted k v)
pempty = punsafeDowncast pnil

{- | Given a comparison function and a "zero" value, check whether a binary relation holds over
2 sorted 'PMap's.

= Important note

This is primarily intended to be used with 'PValue'. We assume that the comparison behaves like
a comparison would (thus, being at least a partial order, or possibly a total order or
equivalence), and that the starting value does not break it. Use with extreme care.

@since 2.0.0
-}
pcheckBinRel ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  (POrd k, PIsData k, PIsData v) =>
  Term
    s
    ( (v :--> v :--> PBool)
        :--> v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f z m1 m2 ->
    let inner = pfix #$ plam $ \self l1 l2 ->
          pelimList
            ( \x xs ->
                plet (pfromData $ psndBuiltin # x) $ \v1 ->
                  pelimList
                    ( \y ys -> unTermCont $ do
                        v2 <- tcont . plet . pfromData $ psndBuiltin # y
                        k1 <- tcont . plet . pfromData $ pfstBuiltin # x
                        k2 <- tcont . plet . pfromData $ pfstBuiltin # y
                        pure
                          $ pif
                            (k1 #== k2)
                            ( f
                                # v1
                                # v2
                                #&& self
                                # xs
                                # ys
                            )
                          $ pif
                            (k1 #< k2)
                            (f # v1 # z #&& self # xs # l2)
                          $ f
                            # z
                            # v2
                            #&& self
                            # l1
                            # ys
                    )
                    ( f
                        # v1
                        # z
                        #&& PPrelude.pall
                        # plam (\p -> f # pfromData (psndBuiltin # p) # z)
                        # xs
                    )
                    l2
            )
            (PPrelude.pall # plam (\p -> f # z #$ pfromData $ psndBuiltin # p) # l2)
            l1
     in inner # pto m1 # pto m2

{- | Verifies all values in the map satisfy the given predicate.

@since 2.0.0
-}
pall ::
  forall (any :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S).
  PIsData v =>
  Term s ((v :--> PBool) :--> PMap any k v :--> PBool)
pall = phoistAcyclic $
  plam $ \pred m ->
    List.pall # plam (\pair -> pred #$ pfromData $ psndBuiltin # pair) # pto m

{- | Build the union of two 'PMap's, merging values that share the same key using the
given function.

@since 2.0.0
-}
punionResolvingCollisionsWith ::
  forall (k :: S -> Type) (v :: S -> Type) (s :: S).
  (POrd k, PIsData k, PIsData v) =>
  Commutativity ->
  Term s ((v :--> v :--> v) :--> PMap 'Sorted k v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
punionResolvingCollisionsWith commutativity =
  phoistAcyclic $
    plam $
      pzipWith . unionMergeHandler commutativity

{- | Maps and filters the map, much like 'Data.List.mapMaybe'.

@since 2.0.0
-}
pmapMaybe ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((a :--> PMaybe b) :--> PMap g k a :--> PMap g k b)
pmapMaybe = phoistAcyclic $
  plam $ \f -> pmapMaybeData #$ plam $ \v -> pmatch (f # pfromData v) $ \case
    PNothing -> pcon PNothing
    PJust v' -> pcon $ PJust (pdata v')

{- | As 'pmapMaybe', but over Data representation.

@since 2.0.0
-}
pmapMaybeData ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s ((PAsData a :--> PMaybe (PAsData b)) :--> PMap g k a :--> PMap g k b)
pmapMaybeData = phoistAcyclic $
  plam $ \f m ->
    pcon . PMap $
      precList
        ( \self x xs ->
            plet (self # xs) $ \xs' ->
              pmatch (f #$ psndBuiltin # x) $ \case
                PNothing -> xs'
                PJust v -> pcons # (ppairDataBuiltin # (pfstBuiltin # x) # v) # xs'
        )
        (const pnil)
        # pto m

{- | Tests whether the map is empty.

@since 2.0.0
-}
pnull ::
  forall (any :: KeyGuarantees) (k :: S -> Type) (v :: S -> Type) (s :: S).
  Term s (PMap any k v :--> PBool)
pnull = plam (\m -> List.pnull # pto m)

{- | Applies a function to every value in the map, much like 'Data.List.map'.

@since 2.0.0
-}
pmap ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((a :--> b) :--> PMap g k a :--> PMap g k b)
pmap = phoistAcyclic $
  plam $
    \f -> pmapData #$ plam $ \v -> pdata (f # pfromData v)

{- | As 'pmap', but over Data representations.

@since 2.0.0
-}
pmapData ::
  forall (g :: KeyGuarantees) (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s ((PAsData a :--> PAsData b) :--> PMap g k a :--> PMap g k b)
pmapData = phoistAcyclic $
  plam $ \f m ->
    pcon . PMap $
      precList
        ( \self x xs ->
            pcons
              # (ppairDataBuiltin # (pfstBuiltin # x) # (f #$ psndBuiltin # x))
              # (self # xs)
        )
        (const pnil)
        # pto m

-- Helpers

-- TODO: Get rid of this whole mess.
type SomeMergeHandler (k :: S -> Type) (v :: S -> Type) (s :: S) =
  SomeMergeHandler_ (->) (Term s k) (Term s v)

-- Signals how to handle value merging for matching keys in 'pzipWith'.
data SomeMergeHandler_ (f :: Type -> Type -> Type) (k :: Type) (v :: Type)
  = SomeMergeHandlerCommutative (MergeHandlerCommutative_ f k v)
  | SomeMergeHandler (MergeHandler_ f k v)

deriving stock instance
  (forall a b. Show (f a b)) =>
  Show (SomeMergeHandler_ f k v)

type MergeHandler (k :: S -> Type) (v :: S -> Type) (s :: S) =
  MergeHandler_ (->) (Term s k) (Term s v)

{- Signals how to handle value merging for matching keys in 'pzipWith'.

 No restrictions on commutativity: Safe to use for both commutative and
 non-commutative merging operations.
-}
data MergeHandler_ (f :: Type -> Type -> Type) (k :: Type) (v :: Type) = MergeHandler
  { mhBoth :: BothPresentHandler_ f k v
  , mhLeft :: OnePresentHandler_ f k v
  , mhRight :: OnePresentHandler_ f k v
  }

deriving stock instance
  (forall a b. Show (f a b)) =>
  Show (MergeHandler_ f k v)

type MergeHandlerCommutative (k :: S -> Type) (v :: S -> Type) (s :: S) =
  MergeHandlerCommutative_ (->) (Term s k) (Term s v)

{- Signals how to handle value merging for matching keys in 'pzipWith'.

 Safe to use for commutative merging operations only.
-}
data MergeHandlerCommutative_ (f :: Type -> Type -> Type) (k :: Type) (v :: Type) = MergeHandlerCommutative
  { mhcBoth :: BothPresentHandlerCommutative_ f k v
  , mhcOne :: OnePresentHandler_ f k v
  }

deriving stock instance
  (forall a b. Show (f a b)) =>
  Show (MergeHandlerCommutative_ f k v)

type BothPresentHandler (k :: S -> Type) (v :: S -> Type) (s :: S) =
  BothPresentHandler_ (->) (Term s k) (Term s v)

data BothPresentHandler_ (f :: Type -> Type -> Type) (k :: Type) (v :: Type)
  = DropBoth
  | -- True ~ left, False ~ right
    PassArg Bool
  | HandleBoth (k `f` (v `f` (v `f` v)))

deriving stock instance
  (forall a b. Show (f a b)) =>
  Show (BothPresentHandler_ f k v)

type BothPresentHandlerCommutative (k :: S -> Type) (v :: S -> Type) (s :: S) =
  BothPresentHandlerCommutative_ (->) (Term s k) (Term s v)

data BothPresentHandlerCommutative_ (f :: Type -> Type -> Type) (k :: Type) (v :: Type)
  = DropBothCommutative
  | HandleBothCommutative (k `f` (v `f` (v `f` v)))

deriving stock instance
  (forall a b. Show (f a b)) =>
  Show (BothPresentHandlerCommutative_ f k v)

type OnePresentHandler (k :: S -> Type) (v :: S -> Type) (s :: S) =
  OnePresentHandler_ (->) (Term s k) (Term s v)

data OnePresentHandler_ (f :: Type -> Type -> Type) (k :: Type) (v :: Type)
  = DropOne
  | PassOne
  | HandleOne (k `f` (v `f` v))

deriving stock instance
  (forall a b. Show (f a b)) =>
  Show (OnePresentHandler_ f k v)

unionMergeHandler ::
  forall (s :: S) (k :: PType) (v :: PType).
  Commutativity ->
  Term s (v :--> (v :--> v)) ->
  SomeMergeHandler k v s
unionMergeHandler NonCommutative merge =
  SomeMergeHandler $ MergeHandler (HandleBoth \_ vl vr -> merge # vl # vr) PassOne PassOne
unionMergeHandler Commutative merge =
  SomeMergeHandlerCommutative $
    MergeHandlerCommutative (HandleBothCommutative \_ vl vr -> merge # vl # vr) PassOne

{- Zip two 'PMap's, using a 'SomeMergeHandler' to decide how to merge based on key
 presence on the left and right.

 Note that using a 'MergeHandlerCommutative' is less costly than a 'MergeHandler'.
-}
pzipWith ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k, PIsData v) =>
  SomeMergeHandler k v s ->
  Term
    s
    ( PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWith (SomeMergeHandler mh) =
  pzipWithData (SomeMergeHandler $ mergeHandlerOnData mh)
pzipWith (SomeMergeHandlerCommutative mhc) =
  pzipWithData (SomeMergeHandlerCommutative $ mergeHandlerCommutativeOnData mhc)

{- Zip two 'PMap's, using a 'SomeMergeHandler' to decide how to merge based on key
 presence on the left and right.

 Note that using a 'MergeHandlerCommutative' is less costly than a 'MergeHandler'.
-}
pzipWithData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  SomeMergeHandler (PAsData k) (PAsData v) s ->
  Term
    s
    ( PMap 'Sorted k v
        :--> PMap 'Sorted k v
        :--> PMap 'Sorted k v
    )
pzipWithData (SomeMergeHandler mh@(MergeHandler _ _ rightPresent)) =
  plam $ \x y ->
    pcon $ PMap $ zipMerge rightPresent (zipMergeInsert mh) # pto x # pto y
pzipWithData (SomeMergeHandlerCommutative mh@(MergeHandlerCommutative _ onePresent)) =
  plam $ \x y ->
    pcon $ PMap $ zipMergeCommutative onePresent (zipMergeInsertCommutative mh) # pto x # pto y

mergeHandlerOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  MergeHandler k v s ->
  MergeHandler (PAsData k) (PAsData v) s
mergeHandlerOnData (MergeHandler bothPresent leftPresent rightPresent) =
  MergeHandler (bothPresentOnData bothPresent) (onePresentOnData leftPresent) (onePresentOnData rightPresent)

mergeHandlerCommutativeOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  MergeHandlerCommutative k v s ->
  MergeHandlerCommutative (PAsData k) (PAsData v) s
mergeHandlerCommutativeOnData (MergeHandlerCommutative bothPresent onePresent) =
  MergeHandlerCommutative (bothPresentCommutativeOnData bothPresent) (onePresentOnData onePresent)

zipMerge ::
  forall (s :: S) (k :: PType) (v :: PType).
  OnePresentHandler (PAsData k) (PAsData v) s ->
  Term
    s
    ( PSBool
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    ) ->
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMerge rightPresent mergeInsertRec = plam $ \ls rs -> pmatch ls $ \case
  PNil ->
    case rightPresent of
      DropOne -> pcon PNil
      PassOne -> rs
      HandleOne handler ->
        List.pmap
          # plam
            ( \pair ->
                plet (pfstBuiltin # pair) \k ->
                  ppairDataBuiltin # k # handler k (psndBuiltin # pair)
            )
          # rs
  PCons l ls' -> mergeInsertRec # pstrue # l # ls' # rs

zipMergeInsert ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  MergeHandler (PAsData k) (PAsData v) s ->
  -- | 'PSTrue' means first arg is left, second arg is right.
  -- The first list gets passed in deconstructed form as head and tail
  -- separately.
  Term
    s
    ( PSBool
        :--> PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeInsert (MergeHandler bothPresent leftPresent rightPresent) = unTermCont $ do
  -- deduplicates all the zipMerge calls through plet, almost as good as hoisting
  zipMerge' <- tcont $ plet $ plam $ zipMerge rightPresent
  pure $ pfix #$ plam $ \self argOrder' x xs' ys -> unTermCont $ do
    let zipMergeRec = zipMerge' # self
        xs = pcons # x # xs'
        zipMergeOrdered xSide ySide =
          pmatch argOrder' $ \argOrder -> applyOrder argOrder zipMergeRec xSide ySide
    pure $ pmatch ys $ \case
      PNil -> pmatch argOrder' $ \argOrder ->
        -- picking handler for presence of x-side only
        case branchOrder argOrder leftPresent rightPresent of
          DropOne -> pcon PNil
          PassOne -> pcons # x # xs'
          HandleOne handler ->
            List.pmap
              # plam
                ( \pair ->
                    plet (pfstBuiltin # pair) \k ->
                      ppairDataBuiltin # k # handler k (psndBuiltin # pair)
                )
              # xs
      PCons y1 ys' -> unTermCont $ do
        y <- tcont $ plet y1
        xk <- tcont $ plet (pfstBuiltin # x)
        yk <- tcont $ plet (pfstBuiltin # y)
        pure $
          pif
            (xk #== yk)
            ( case bothPresent of
                DropBoth -> zipMergeOrdered xs' ys'
                PassArg passLeft -> pmatch argOrder' $ \argOrder ->
                  pcons
                    # (if passLeft == (argOrder == PSTrue) then x else y)
                    # applyOrder argOrder zipMergeRec xs' ys'
                HandleBoth merge -> pmatch argOrder' $ \argOrder ->
                  pcons
                    # ( ppairDataBuiltin
                          # xk
                          #$ applyOrder' argOrder (merge xk) (psndBuiltin # x) (psndBuiltin # y)
                      )
                    #$ applyOrder argOrder zipMergeRec xs' ys'
            )
            ( pif
                (pfromData xk #< pfromData yk)
                ( pmatch argOrder' $ \argOrder ->
                    -- picking handler for presence of only x-side
                    case branchOrder argOrder leftPresent rightPresent of
                      DropOne -> self # branchOrder argOrder psfalse pstrue # y # ys' # xs'
                      PassOne -> pcons # x # applyOrder argOrder zipMergeRec xs' ys
                      HandleOne handler ->
                        pcons
                          # (ppairDataBuiltin # xk # handler xk (psndBuiltin # x))
                          # (self # branchOrder argOrder psfalse pstrue # y # ys' # xs')
                )
                ( pmatch argOrder' $ \argOrder ->
                    -- picking handler for presence of only y-side
                    case branchOrder argOrder rightPresent leftPresent of
                      DropOne -> self # argOrder' # x # xs' # ys'
                      PassOne -> pcons # y # applyOrder argOrder zipMergeRec xs ys'
                      HandleOne handler ->
                        pcons
                          # (ppairDataBuiltin # yk # handler yk (psndBuiltin # y))
                          # (self # argOrder' # x # xs' # ys')
                )
            )

zipMergeCommutative ::
  forall (s :: S) (k :: PType) (v :: PType).
  OnePresentHandler (PAsData k) (PAsData v) s ->
  Term
    s
    ( PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    ) ->
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeCommutative onePresent mergeInsertRec = plam $ \ls rs -> pmatch ls $ \case
  PNil ->
    case onePresent of
      DropOne -> pcon PNil
      PassOne -> rs
      HandleOne handler ->
        List.pmap
          # plam
            ( \pair ->
                plet (pfstBuiltin # pair) \k ->
                  ppairDataBuiltin # k # handler k (psndBuiltin # pair)
            )
          # rs
  PCons l ls' -> mergeInsertRec # l # ls' # rs

zipMergeInsertCommutative ::
  forall (s :: S) (k :: PType) (v :: PType).
  (POrd k, PIsData k) =>
  MergeHandlerCommutative (PAsData k) (PAsData v) s ->
  -- | 'PSTrue' means first arg is left, second arg is right.
  -- The first list gets passed in deconstructed form as head and tail
  -- separately.
  Term
    s
    ( PBuiltinPair (PAsData k) (PAsData v)
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
        :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
    )
zipMergeInsertCommutative (MergeHandlerCommutative bothPresent onePresent) = unTermCont $ do
  -- deduplicates all the zipMerge calls through plet, almost as good as hoisting
  zipMerge' <- tcont $ plet $ plam $ zipMergeCommutative onePresent
  pure $ pfix #$ plam $ \self x xs' ys -> unTermCont $ do
    let zipMergeRec = zipMerge' # self
        xs = pcons # x # xs'
    pure $ pmatch ys $ \case
      PNil ->
        -- picking handler for presence of x-side only
        case onePresent of
          DropOne -> pcon PNil
          PassOne -> pcons # x # xs'
          HandleOne handler ->
            List.pmap
              # plam
                ( \pair ->
                    plet (pfstBuiltin # pair) \k ->
                      ppairDataBuiltin # k # handler k (psndBuiltin # pair)
                )
              # xs
      PCons y1 ys' -> unTermCont $ do
        y <- tcont $ plet y1
        xk <- tcont $ plet (pfstBuiltin # x)
        yk <- tcont $ plet (pfstBuiltin # y)
        pure $
          pif
            (xk #== yk)
            ( case bothPresent of
                DropBothCommutative -> zipMergeRec # xs' # ys'
                HandleBothCommutative merge ->
                  pcons
                    # ( ppairDataBuiltin
                          # xk
                          #$ merge xk (psndBuiltin # x) (psndBuiltin # y)
                      )
                    #$ zipMergeRec
                    # xs'
                    # ys'
            )
            ( pif
                (pfromData xk #< pfromData yk)
                ( case onePresent of
                    DropOne -> self # y # ys' # xs'
                    PassOne -> pcons # x # (zipMergeRec # xs' # ys)
                    HandleOne handler ->
                      pcons
                        # (ppairDataBuiltin # xk # handler xk (psndBuiltin # x))
                        # (self # y # ys' # xs')
                )
                ( case onePresent of
                    DropOne -> self # x # xs' # ys'
                    PassOne -> pcons # y # (zipMergeRec # ys' # xs)
                    HandleOne handler ->
                      pcons
                        # (ppairDataBuiltin # yk # handler yk (psndBuiltin # y))
                        # (self # x # xs' # ys')
                )
            )

bothPresentOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  BothPresentHandler k v s ->
  BothPresentHandler (PAsData k) (PAsData v) s
bothPresentOnData = \case
  DropBoth -> DropBoth
  PassArg arg -> PassArg arg
  HandleBoth f -> HandleBoth \k x y -> pdata $ f (pfromData k) (pfromData x) (pfromData y)

onePresentOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  OnePresentHandler k v s ->
  OnePresentHandler (PAsData k) (PAsData v) s
onePresentOnData = \case
  DropOne -> DropOne
  PassOne -> PassOne
  HandleOne f -> HandleOne \x y -> pdata $ f (pfromData x) (pfromData y)

bothPresentCommutativeOnData ::
  forall (s :: S) (k :: PType) (v :: PType).
  (PIsData k, PIsData v) =>
  BothPresentHandlerCommutative k v s ->
  BothPresentHandlerCommutative (PAsData k) (PAsData v) s
bothPresentCommutativeOnData = \case
  DropBothCommutative -> DropBothCommutative
  HandleBothCommutative f ->
    HandleBothCommutative \k x y -> pdata $ f (pfromData k) (pfromData x) (pfromData y)

-- | Apply given Plutarch fun with given reified (on Haskell-level) arg order.
applyOrder ::
  forall (s :: S) (a :: PType) (b :: PType).
  -- | 'PSTrue' means first arg is left, second arg is right.
  PSBool s ->
  -- | A function that expects argument order 'left right'.
  Term s (a :--> a :--> b) ->
  -- | First arg.
  Term s a ->
  -- | Second arg.
  Term s a ->
  Term s b
applyOrder argOrder pfun a b = branchOrder argOrder (pfun # a # b) (pfun # b # a)

branchOrder :: forall (s :: S) (a :: Type). PSBool s -> a -> a -> a
branchOrder PSTrue t _ = t
branchOrder PSFalse _ f = f

applyOrder' ::
  forall (s :: S) (a :: Type) (b :: Type).
  -- | 'PSTrue' means first arg is left, second arg is right.
  PSBool s ->
  -- | A function that expects argument order 'left right'.
  (a -> a -> b) ->
  -- | First arg.
  a ->
  -- | Second arg.
  a ->
  b
applyOrder' argOrder fun a b = branchOrder argOrder (fun a b) (fun b a)
