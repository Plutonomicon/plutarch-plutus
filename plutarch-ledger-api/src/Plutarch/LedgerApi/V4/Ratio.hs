{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- Mirrors the equivalent V4 module in plutus-ledger-api
module Plutarch.LedgerApi.V4.Ratio (
  PRationalData (..),
  prationalFromData,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (
  LiftError (CouldNotDecodeData),
  punsafeCoercePLifted,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V4 qualified as Plutus

-- | @since 3.8.0
data PRationalData (s :: S) = PRationalData
  { prationalData'numerator :: Term s (PAsData PInteger)
  , prationalData'denominator :: Term s (PAsData PPositive)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    )
    via (DeriveAsDataRec PRationalData)

-- | @since 3.8.0
instance POrd PRationalData where
  (#<=) = liftCompareOp (#<=)
  (#<) = liftCompareOp (#<)

-- | @since 3.8.0
instance PLiftable PRationalData where
  type AsHaskell PRationalData = Plutus.Rational
  type PlutusRepr PRationalData = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PRationalData is list-represented."

{- | Checks that we have a @Data@-encoded list with exactly two elements, that
the first element is a 'PInteger', that the second is a 'PPositive', and that
the fraction they form is in reduced form.

@since 3.8.0
-}
instance PValidateData PRationalData where
  pwithValidated d r = plet (pasList # d) $ \ell -> pheadTailBuiltin ell $
    \h1 t1 -> pheadTailBuiltin t1 $ \h2 t2 -> pmatch t2 $ \case
      PNil ->
        let num = pparseData @PInteger h1
            den = pparseData @PPositive h2
         in pif
              ((pgcd # pfromData num # pto (pto (pfromData den))) #== pconstant 1)
              r
              perror
      PCons _ _ -> perror

-- | @since 3.8.0
instance PTryFrom PData (PAsData PRationalData) where
  ptryFrom' opq = runTermCont $ do
    ell <- pletC $ pasList # opq
    (h1, t1) <- pheadTailC ell
    (h2, t2) <- pheadTailC t1
    num <- pletC $ ptryFrom @(PAsData PInteger) h1 fst
    den <- pletC $ ptryFrom @(PAsData PPositive) h2 fst
    pguardC "ptryFrom(PRationalData): too many fields" $ pmatch t2 $ \case PNil -> pcon PTrue; PCons _ _ -> pcon PFalse
    pguardC "ptryFrom(PRationalData): un-reduced fraction" $ (pgcd # pfromData num # pto (pto (pfromData den))) #== pconstant 1
    pure (punsafeCoerce opq, ())

-- | @since 3.8.0
prationalFromData :: forall (s :: S). Term s (PRationalData :--> PRational)
prationalFromData = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    PRationalData n d <- pmatchC x
    pure . pcon $ PRational (pfromData n) (pfromData d)

-- Helpers

pheadTailC ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBuiltinList a) ->
  TermCont s (Term s a, Term s (PBuiltinList a))
pheadTailC ell = tcont (pheadTailBuiltin ell . curry)

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
      PRationalData ln ld <- pmatchC l
      PRationalData rn rd <- pmatchC r
      pure $ f (pfromData ln * pupcast (pfromData rd)) (pfromData rn * pupcast (pfromData ld))

pgcd :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont . plet $ pabs # x'
    y <- tcont . plet $ pabs # y'
    pure $
      pif
        (x #<= y)
        (pgcd' # y # x)
        (pgcd' # x # y)

-- assumes inputs are non negative and a >= b
pgcd' :: Term s (PInteger :--> PInteger :--> PInteger)
pgcd' = phoistAcyclic $ pfix (plam . f)
  where
    f self a b =
      pif
        (b #== 0)
        a
        $ self # b #$ pmod # a # b
