{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Useful tools that aren't part of the Plutarch API per se, but get used in
multiple places.
-}
module Plutarch.LedgerApi.Utils (
  -- * Types
  Mret (..),
  PMaybeData (..),
  PRationalData (..),

  -- * Functions

  -- ** PMaybeData
  pfromDJust,
  pisDJust,
  pmaybeData,
  pdjust,
  pdnothing,
  pmaybeToMaybeData,
  passertPDJust,

  -- ** PRationalData
  prationalFromData,
) where

import Data.Bifunctor (first)
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
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

-- | @since 2.0.0
data PMaybeData (a :: S -> Type) (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PMaybeData a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PMaybeData a)

-- | @since 2.0.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PMaybeData a))

-- | @since 2.0.0
instance PLiftData a => PUnsafeLiftDecl (PMaybeData a) where
  type PLifted (PMaybeData a) = Maybe (PLifted a)

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Maybe a) (PMaybeData (PConstanted a)))
  instance
    PConstantData a => PConstantDecl (Maybe a)

-- Have to manually write this instance because the constructor id ordering is screwed for 'Maybe'....

-- | @since 2.0.0
instance (PIsData a, POrd a) => PPartialOrd (PMaybeData a) where
  x #< y = pmaybeLT False (#<) # x # y
  x #<= y = pmaybeLT True (#<=) # x # y

-- | @since 2.0.0
instance (PIsData a, POrd a) => POrd (PMaybeData a)

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
instance PPartialOrd PRationalData where
  (#<=) = liftCompareOp (#<=)
  (#<) = liftCompareOp (#<)

-- | @since 3.1.0
instance POrd PRationalData

-- | @since 3.1.0
instance DerivePlutusType PRationalData where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance PUnsafeLiftDecl PRationalData where type PLifted PRationalData = Plutus.Rational

-- | @since 3.1.0
deriving via (DerivePConstantViaData Plutus.Rational PRationalData) instance PConstantDecl Plutus.Rational

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
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybeData a :--> a)
pfromDJust = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PDNothing _ -> ptraceInfoError "pfromDJust: found PDNothing"
    PDJust x -> pfromData $ pfield @"_0" # x

{- | Yield 'PTrue' if a given 'PMaybeData' is of the form @'PDJust' _@.

@since 2.1.1
-}
pisDJust ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x -> pmatch x $ \case
    PDJust _ -> pconstant True
    _ -> pconstant False

{- | Special version of 'pmaybe' that works with 'PMaybeData'.

@since 2.1.1
-}
pmaybeData ::
  forall (a :: PType) (b :: PType) (s :: S).
  PIsData a =>
  Term s (b :--> (a :--> b) :--> PMaybeData a :--> b)
pmaybeData = phoistAcyclic $
  plam $ \d f m -> pmatch m $
    \case
      PDJust x -> f #$ pfield @"_0" # x
      _ -> d

{- | Construct a 'PDJust' value.

@since 2.1.1
-}
pdjust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (a :--> PMaybeData a)
pdjust = phoistAcyclic $
  plam $
    \x -> pcon $ PDJust $ pdcons @"_0" # pdata x #$ pdnil

{- | Construct a 'PDNothing' value.

@since 2.1.1
-}
pdnothing ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a)
pdnothing = phoistAcyclic $ pcon $ PDNothing pdnil

{- | Construct a 'PMaybeData' given a 'PMaybe'. Could be useful if you want to
"lift" from 'PMaybe' to 'Maybe'.

@since 2.1.1
-}
pmaybeToMaybeData ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PMaybe a :--> PMaybeData a)
pmaybeToMaybeData = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PNothing -> pcon $ PDNothing pdnil
    PJust x -> pcon $ PDJust $ pdcons @"_0" # pdata x # pdnil

{- | Extract the value stored in a 'PMaybeData' container. If there's no value,
throw an error with the given message.

@since 2.1.1
-}
passertPDJust ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (PString :--> PMaybeData a :--> a)
passertPDJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PDJust ((pfield @"_0" #) -> v) -> v
    _ -> ptraceInfoError emsg

-- Helpers

pmaybeLT ::
  forall (a :: S -> Type) (s :: S).
  Bool ->
  ( forall (s' :: S) (rec_ :: [PLabeledType]).
    rec_ ~ '["_0" ':= a] =>
    Term s' (PDataRecord rec_) ->
    Term s' (PDataRecord rec_) ->
    Term s' PBool
  ) ->
  Term s (PMaybeData a :--> PMaybeData a :--> PBool)
pmaybeLT whenBothNothing ltF = phoistAcyclic $
  plam $ \x y -> unTermCont $ do
    a <- tcont . plet $ pasConstr #$ pforgetData $ pdata x
    b <- tcont . plet $ pasConstr #$ pforgetData $ pdata y
    cid1 <- tcont . plet $ pfstBuiltin # a
    cid2 <- tcont . plet $ pfstBuiltin # b
    pure
      $ pif
        (cid1 #< cid2)
        (pconstant False)
      $ pif
        (cid1 #== cid2)
        {- Some hand optimization here: usually, the fields would be 'plet'ed here if using 'POrd' derivation
          machinery. However, in this case - there's no need for the fields for the 'Nothing' case.

          Would be nice if this could be done on the auto derivation case....
        -}
        ( pif
            (cid1 #== 0)
            (ltF (punsafeCoerce $ psndBuiltin # a) (punsafeCoerce $ psndBuiltin # b))
            -- Both are 'Nothing'. Let caller choose answer.
            $ pconstant whenBothNothing
        )
      $ pconstant True

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
