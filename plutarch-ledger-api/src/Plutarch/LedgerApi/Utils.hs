{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Useful tools that aren't part of the Plutarch API per se, but get used in
multiple places.
-}
module Plutarch.LedgerApi.Utils (
  -- * Types
  Mret (..),
  PMaybeData (..),
  PRationalData (..),
  PSBool (..),

  -- * Functions

  -- ** PMaybeData
  pfromDJust,
  pisDJust,
  pmaybeData,
  pdjust,
  pdnothing,
  pmaybeToMaybeData,
  pmaybeDataToMaybe,
  passertPDJust,

  -- ** PRationalData
  prationalFromData,

  -- ** PSBool
  pmatchStrict,
  pstrue,
  psfalse,
  psif,
  psif',
  psnot,
  psand,
  psand',
  psor,
  psor',
) where

import Data.Bifunctor (first)
import Plutarch.Builtin (
  PIsData (pdataImpl, pfromDataImpl),
  pasConstr,
  pconstrBuiltin,
  pforgetData,
 )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Internal.Lift (DeriveDataPLiftable)
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 qualified as Plutus

{- | 'Term', but with its type arguments flipped. This is a useful helper for
defining 'PTryFrom' instances.

For example, consider the 'PTryFrom' instance for 'PTokenName':

@
instance PTryFrom PData (PAsData PTokenName) where
   type PTryFromExcess PData (PAsData PTokenName) = Mret PTokenName
@

We need to do this because 'PTryFromExcess' expects something of kind @S ->
Type@, but 'Term' has kind @S -> (S -> Type) -> Type@, which doesn't quite
fit. By using 'Mret', we end up with something of kind @(S -> Type)
-> S -> Type@, which fits.

The name is just 'Term' written backwards.

@since 2.0.0
-}
newtype Mret (a :: S -> Type) (s :: S) = Mret (Term s a)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )

-- | @since WIP
data PMaybeData (a :: S -> Type) (s :: S)
  = PDJust (Term s (PAsData a))
  | PDNothing
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since WIP
instance PlutusType (PMaybeData a) where
  type PInner (PMaybeData a) = PData
  {-# INLINEABLE pcon' #-}
  pcon' = \case
    PDJust t ->
      pforgetData $ pconstrBuiltin # 0 #$ pcons # pforgetData t # pnil
    PDNothing ->
      pforgetData $ pconstrBuiltin # 1 # pnil
  {-# INLINEABLE pmatch' #-}
  pmatch' t f = plet (pasConstr # t) $ \asConstr ->
    pif
      ((pfstBuiltin # asConstr) #== 1)
      (f PDNothing)
      (f . PDJust . punsafeCoerce $ phead #$ psndBuiltin # asConstr)

-- | @since WIP
deriving via
  DeriveDataPLiftable (PMaybeData a) (Maybe (AsHaskell a))
  instance
    (Plutus.ToData (AsHaskell a), Plutus.FromData (AsHaskell a)) => PLiftable (PMaybeData a)

-- | @since WIP
instance PIsData (PMaybeData a) where
  {-# INLINEABLE pdataImpl #-}
  pdataImpl = pto
  {-# INLINEABLE pfromDataImpl #-}
  pfromDataImpl = punsafeCoerce

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PMaybeData a)

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PMaybeData a))

-- | @since 2.0.0
instance (PIsData a, POrd a) => POrd (PMaybeData a) where
  {-# INLINEABLE (#<=) #-}
  t1 #<= t2 = pmatch t1 $ \case
    PDNothing -> pcon PTrue
    PDJust t1' -> pmatch t2 $ \case
      PDNothing -> pcon PFalse
      PDJust t2' -> pfromData t1' #<= pfromData t2'
  {-# INLINEABLE (#<) #-}
  t1 #< t2 = pmatch t1 $ \case
    PDNothing -> pmatch t2 $ \case
      PDNothing -> pcon PFalse
      PDJust _ -> pcon PTrue
    PDJust t1' -> pmatch t2 $ \case
      PDNothing -> pcon PFalse
      PDJust t2' -> pfromData t1' #< pfromData t2'
  {-# INLINEABLE pmin #-}
  pmin t1 t2 = pmatch t1 $ \case
    PDNothing -> t1
    PDJust t1' -> pmatch t2 $ \case
      PDNothing -> t2
      PDJust t2' ->
        pif
          (pfromData t1' #< pfromData t2')
          t1
          t2
  {-# INLINEABLE pmax #-}
  pmax t1 t2 = pmatch t1 $ \case
    PDNothing -> t2
    PDJust t1' -> pmatch t2 $ \case
      PDNothing -> t1
      PDJust t2' ->
        pif
          (pfromData t1' #< pfromData t2')
          t2
          t1

{- | A Rational type that corresponds to the data encoding used by 'Plutus.Rational'.

@since 3.1.0
-}
newtype PRationalData s
  = PRationalData
      ( Term
          s
          ( PDataRecord
              '[ "numerator" ':= PInteger
               , "denominator" ':= PPositive
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance POrd PRationalData where
  {-# INLINEABLE (#<=) #-}
  (#<=) = liftCompareOp (#<=)
  {-# INLINEABLE (#<) #-}
  (#<) = liftCompareOp (#<)

-- | @since 3.1.0
instance DerivePlutusType PRationalData where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PRationalData Plutus.Rational
  instance
    PLiftable PRationalData

-- | @since 3.1.0
instance PTryFrom PData PRationalData where
  type PTryFromExcess PData PRationalData = Mret PPositive
  ptryFrom' opq cont = ptryFrom @(PAsData PRationalData) opq (cont . first punsafeCoerce)

{- | This instance produces a verified positive denominator as the excess output.

@since 3.1.0
-}
instance PTryFrom PData (PAsData PRationalData) where
  type PTryFromExcess PData (PAsData PRationalData) = Mret PPositive
  ptryFrom' opq = runTermCont $ do
    opq' <- pletC $ pasConstr # opq
    pguardC "ptryFrom(PRationalData): invalid constructor id" $ pfstBuiltin # opq' #== 0
    flds <- pletC $ psndBuiltin # opq'
    _numr <- pletC $ ptryFrom @(PAsData PInteger) (phead # flds) snd
    ratTail <- pletC $ ptail # flds
    denm <- pletC $ ptryFrom @(PAsData PPositive) (phead # ratTail) snd
    pguardC "ptryFrom(PRationalData): constructor fields len > 2" $ ptail # ratTail #== pnil
    pure (punsafeCoerce opq, denm)

-- | @since 3.1.0
prationalFromData :: ClosedTerm (PRationalData :--> PRational)
prationalFromData = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    l <- pletFieldsC @'["numerator", "denominator"] x
    pure . pcon $ PRational (getField @"numerator" l) (getField @"denominator" l)

{- | Extracts the element out of a 'PDJust' and throws an error if its
argument is 'PDNothing'.

@since 2.1.1
-}
pfromDJust ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PMaybeData a :--> a)
pfromDJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PDNothing -> ptraceInfoError "pfromDJust: found PDNothing"
    PDJust x -> pfromData x

{- | Yield 'PTrue' if a given 'PMaybeData' is of the form @'PDJust' _@.

@since 2.1.1
-}
pisDJust ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    PDJust _ -> pconstant True
    _ -> pconstant False

{- | Special version of 'pmaybe' that works with 'PMaybeData'.

@since 2.1.1
-}
pmaybeData ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PIsData a =>
  Term s (b :--> (a :--> b) :--> PMaybeData a :--> b)
pmaybeData = phoistAcyclic $
  plam $ \d f m -> pmatch m $
    \case
      PDJust x -> f # pfromData x
      _ -> d

{- | Construct a 'PDJust' value.

@since 2.1.1
-}
pdjust ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s (a :--> PMaybeData a)
pdjust = phoistAcyclic $
  plam $
    \x -> pcon . PDJust . pdata $ x

{- | Construct a 'PDNothing' value.

@since 2.1.1
-}
pdnothing ::
  forall (a :: S -> Type) (s :: S).
  Term s (PMaybeData a)
pdnothing = pcon PDNothing

{- | Construct a 'PMaybeData' given a 'PMaybe'. Could be useful if you want to
"lift" from 'PMaybe' to 'Maybe'.

@since 2.1.1
-}
pmaybeToMaybeData ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PMaybe a :--> PMaybeData a)
pmaybeToMaybeData = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> pcon PDNothing
    PJust x -> pcon . PDJust . pdata $ x

{- | Inverse of `pmaybeToMaybeData`

@since WIP
-}
pmaybeDataToMaybe ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PMaybeData a :--> PMaybe a)
pmaybeDataToMaybe = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PDNothing -> pcon PNothing
    PDJust x -> pcon . PJust . pfromData $ x

{- | Extract the value stored in a 'PMaybeData' container. If there's no value,
throw an error with the given message.

@since 2.1.1
-}
passertPDJust ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s (PString :--> PMaybeData a :--> a)
passertPDJust = phoistAcyclic $
  plam $ \emsg t -> pmatch t $ \case
    PDJust t' -> pfromData t'
    PDNothing -> ptraceInfoError emsg

{- | Scott-encoded boolean.

@since WIP
-}
data PSBool (s :: S)
  = PSTrue
  | PSFalse
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    , -- | @since WIP
      Show
    )

-- | @since WIP
instance PlutusType PSBool where
  type PInner PSBool = PForall PSBoolRaw
  {-# INLINEABLE pcon' #-}
  pcon' = \case
    PSTrue -> pcon $ PForall $ pcon $ PSBoolRaw (plam const)
    PSFalse -> pcon $ PForall $ pcon $ PSBoolRaw (plam (const id))
  {-# INLINEABLE pmatch' #-}
  pmatch' x' f =
    pmatch x' $ \(PForall raw) ->
      pmatch raw $ \(PSBoolRaw x) ->
        pforce $ x # pdelay (f PSTrue) # pdelay (f PSFalse)

{- | Strict version of 'pmatch' for 'PSBool'.

@since WIP
-}
pmatchStrict ::
  forall (r :: S -> Type) (s :: S).
  Term s PSBool ->
  (PSBool s -> Term s r) ->
  Term s r
pmatchStrict x' f =
  pmatch (pto x') $ \(PForall raw) ->
    pmatch raw $ \(PSBoolRaw x) ->
      x # f PSTrue # f PSFalse

-- | @since WIP
pstrue :: forall (s :: S). Term s PSBool
pstrue = pcon PSTrue

-- | @since WIP
psfalse :: forall (s :: S). Term s PSBool
psfalse = pcon PSFalse

{- | Strict @if@ on Scott-encoded bool.

@since WIP
-}
psif' ::
  forall (s :: S) (a :: S -> Type).
  Term s PSBool ->
  Term s a ->
  Term s a ->
  Term s a
psif' b t f = pmatchStrict b $ \case
  PSTrue -> t
  PSFalse -> f

{- | Lazy @if@ on Scott-encoded bool.

@since WIP
-}
psif ::
  forall (s :: S) (a :: S -> Type).
  Term s PSBool ->
  Term s a ->
  Term s a ->
  Term s a
psif b t f = pforce $ psif' b (pdelay t) (pdelay f)

{- | @not@ on Scott-encoded bool.

@since WIP
-}
psnot :: forall (s :: S). Term s PSBool -> Term s PSBool
psnot b = psif' b psfalse pstrue

{- | Strict AND on Scott-encoded bool.

@since WIP
-}
psand' :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psand' a b = psif' a b psfalse

-- | Lazy AND on Scott-encoded bool.
psand :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psand a b = psif a b psfalse

{- | Strict OR on Scott-encoded bool.

@since WIP
-}
psor' :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psor' a = psif' a pstrue

{- | Lazy OR on Scott-encoded bool

@since WIP
-}
psor :: forall (s :: S). Term s PSBool -> Term s PSBool -> Term s PSBool
psor a = psif a pstrue

-- Helpers

newtype PSBoolRaw (a :: S -> Type) (s :: S)
  = PSBoolRaw (Term s (a :--> a :--> a))

instance PlutusType (PSBoolRaw a) where
  type PInner (PSBoolRaw a) = a :--> a :--> a
  pcon' (PSBoolRaw x) = x
  pmatch' x f = f (PSBoolRaw x)

liftCompareOp ::
  forall (s :: S).
  (forall (s' :: S). Term s' PInteger -> Term s' PInteger -> Term s' PBool) ->
  Term s PRationalData ->
  Term s PRationalData ->
  Term s PBool
liftCompareOp f x y = phoistAcyclic (plam go) # x # y
  where
    go ::
      forall (s' :: S).
      Term s' PRationalData ->
      Term s' PRationalData ->
      Term s' PBool
    go l r = unTermCont $ do
      l' <- pletFieldsC @'["numerator", "denominator"] l
      r' <- pletFieldsC @'["numerator", "denominator"] r
      let ln = pfromData $ getField @"numerator" l'
      let ld = pfromData $ getField @"denominator" l'
      let rn = pfromData $ getField @"numerator" r'
      let rd = pfromData $ getField @"denominator" r'
      pure $ f (ln * pto rd) (rn * pto ld)
