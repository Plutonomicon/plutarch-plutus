{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.RationalData (
  PRationalData,
  prationalFromData,
) where

import Data.Bifunctor (first)
import Plutarch.Builtin (pasConstr)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import qualified Plutarch.Unsafe as PUNSAFE
import qualified PlutusTx.Ratio as Plutus

-- | A Rational type that corresponds to the data encoding used by 'Plutus.Rational'.
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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

-- | @since 1.2.1
instance PPartialOrd PRationalData where
  (#<=) = liftCompareOp (#<=)
  (#<) = liftCompareOp (#<)

-- | @since 1.2.1
instance POrd PRationalData

instance DerivePlutusType PRationalData where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRationalData where type PLifted PRationalData = Plutus.Rational
deriving via (DerivePConstantViaData Plutus.Rational PRationalData) instance PConstantDecl Plutus.Rational

instance PTryFrom PData PRationalData where
  type PTryFromExcess PData PRationalData = Flip Term PPositive
  ptryFrom' opq cont = ptryFrom @(PAsData PRationalData) opq (cont . first PUNSAFE.punsafeCoerce)

-- | NOTE: This instance produces a verified positive denominator as the excess output.
instance PTryFrom PData (PAsData PRationalData) where
  type PTryFromExcess PData (PAsData PRationalData) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    opq' <- pletC $ pasConstr # opq
    pguardC "ptryFrom(PRationalData): invalid constructor id" $ pfstBuiltin # opq' #== 0
    flds <- pletC $ psndBuiltin # opq'
    _numr <- pletC $ ptryFrom @(PAsData PInteger) (phead # flds) snd
    ratTail <- pletC $ ptail # flds
    denm <- pletC $ ptryFrom @(PAsData PPositive) (phead # ratTail) snd
    pguardC "ptryFrom(PRationalData): constructor fields len > 2" $ ptail # ratTail #== pnil
    pure (PUNSAFE.punsafeCoerce opq, denm)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

prationalFromData :: ClosedTerm (PRationalData :--> PRational)
prationalFromData = phoistAcyclic $
  plam $ \x -> unTermCont $ do
    l <- pletFieldsC @'["numerator", "denominator"] x
    pure . pcon $ PRational (getField @"numerator" l) (getField @"denominator" l)

-- Helpers

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
