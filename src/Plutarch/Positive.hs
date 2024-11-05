{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Positive (
  PPositive,
  ppositive,
  ptryPositive,
  Positive,
  mkPositive,
) where

import Data.Coerce (coerce)
import Data.Functor.Const (Const)
import Data.Text (pack)
import GHC.Generics (Generic)
import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
import Plutarch.Internal.Builtin (PInteger, pif, plam, pto)
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Numeric (PIntegral, PNum (pfromInteger, (#-)))
import Plutarch.Internal.Ord (POrd, PPartialOrd, (#<=))
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType,
  pcon,
 )
import Plutarch.Internal.Show (PShow, pshow)
import Plutarch.Internal.Term (
  Term,
  phoistAcyclic,
  plet,
  pthrow,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.TermCont (runTermCont, tcont)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )
import Test.QuickCheck qualified as QuickCheck

newtype PPositive s = PPositive (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PShow)
instance DerivePlutusType PPositive where type DPTStrat _ = PlutusTypeNewtype

instance PNum PPositive where
  x #- y = ptryPositive #$ pto x #- pto y

  pfromInteger x
    | x <= 0 =
        pthrow $
          "PPositive.pfromInteger: encountered non positive: " <> pack (show x)
    | otherwise = pcon $ PPositive $ pfromInteger x

instance PTryFrom PInteger PPositive where
  type PTryFromExcess PInteger PPositive = Const ()
  ptryFrom' opq = runTermCont $ pure (ptryPositive # opq, ())

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PPositive) where
  type PTryFromExcess PData (PAsData PPositive) = Flip Term PPositive
  ptryFrom' opq = runTermCont $ do
    (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
    res <- tcont . plet $ ptryPositive # i
    resData <- tcont . plet $ pdata res
    pure (resData, res)

-- | @since WIP
instance PUnsafeLiftDecl PPositive where type PLifted PPositive = Positive

-- | @since WIP
deriving via (DerivePConstantDirect Integer PPositive) instance PConstantDecl Positive

-- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is zero.
ppositive :: Term s (PInteger :--> PMaybe PPositive)
ppositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= 0)
      (pcon PNothing)
      $ pcon . PJust . pcon
      $ PPositive i

-- | Partial version of 'PPositive'. Errors if argument is zero.
ptryPositive :: Term s (PInteger :--> PPositive)
ptryPositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= 0)
      (ptraceInfoError $ "ptryPositive: building with non positive: " <> pshow i)
      $ pcon
      $ PPositive i

-- | @since WIP
newtype Positive = UnsafeMkPositive {getPositive :: Integer}
  deriving stock (Show, Eq, Ord)

-- | @since WIP
deriving via (QuickCheck.Positive Integer) instance Arbitrary Positive

-- | @since WIP
deriving via Integer instance CoArbitrary Positive

-- | @since WIP
instance Function Positive where
  {-# INLINEABLE function #-}
  function = functionMap @Integer coerce coerce

-- | @since WIP
deriving via Integer instance Pretty Positive

-- | @since WIP
mkPositive :: Integer -> Maybe Positive
mkPositive n
  | n > 0 = Just $ UnsafeMkPositive n
  | otherwise = Nothing