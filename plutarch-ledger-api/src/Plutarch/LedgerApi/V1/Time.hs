{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Time (
  -- * Type
  PPosixTime (..),

  -- * Functions
  pposixTime,
  unPPosixTime,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import Plutarch.Reducible (Reduce)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 2.0.0
newtype PPosixTime (s :: S) = PPosixTime (Term s PInteger)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PPosixTime)

-- | @since 3.3.0
instance PCountable PPosixTime where
  {-# INLINEABLE psuccessor #-}
  psuccessor = phoistAcyclic $ plam (\x -> x #+ pposixTime pone)
  {-# INLINEABLE psuccessorN #-}
  psuccessorN = phoistAcyclic $ plam $ \p t ->
    let p' = pcon . PPosixTime . pto $ p
     in p' #+ t

-- | @since 3.3.0
instance PEnumerable PPosixTime where
  {-# INLINEABLE ppredecessor #-}
  ppredecessor = phoistAcyclic $ plam (\x -> x #- pposixTime pone)
  {-# INLINEABLE ppredecessorN #-}
  ppredecessorN = phoistAcyclic $ plam $ \p t ->
    let p' = pcon . PPosixTime . pto $ p
     in t #- p'

-- | @since 3.3.0
instance PAdditiveSemigroup PPosixTime where
  {-# INLINEABLE (#+) #-}
  t1 #+ t2 = pposixTime (unPPosixTime t1 #+ unPPosixTime t2)
  {-# INLINEABLE pscalePositive #-}
  pscalePositive t p = pposixTime (unPPosixTime t #* pto p)

-- | @since 3.3.0
instance PAdditiveMonoid PPosixTime where
  {-# INLINEABLE pzero #-}
  pzero = pposixTime pzero
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural t n = pposixTime (unPPosixTime t #* pto n)

-- | @since 3.3.0
instance PAdditiveGroup PPosixTime where
  {-# INLINEABLE pnegate #-}
  pnegate = phoistAcyclic $ plam $ \t -> pposixTime (pnegate # unPPosixTime t)
  {-# INLINEABLE (#-) #-}
  t1 #- t2 = pposixTime (unPPosixTime t1 #- unPPosixTime t2)

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.3.0
deriving via
  DeriveNewtypePLiftable PPosixTime Plutus.POSIXTime
  instance
    PLiftable PPosixTime

-- | @since 3.3.1
instance PTryFrom PData (PAsData PPosixTime) where
  ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    Term s PData ->
    ((Term s (PAsData PPosixTime), Reduce (PTryFromExcess PData (PAsData PPosixTime) s)) -> Term s r) ->
    Term s r
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term s (PAsData PInteger), _) <-
      tcont $ ptryFrom @(PAsData PInteger) opq
    pure (punsafeCoerce wrapped, ())

{- | Construct a 'PPosixTime' from a 'PInteger'. Same as using the constructor,
but a lot shorter.

@since 3.3.0
-}
pposixTime :: forall (s :: S). Term s PInteger -> Term s PPosixTime
pposixTime = pcon . PPosixTime

{- | Unwrap a 'PPosixTime' to get a 'PInteger'. Same as using 'pmatch', but a
lot shorter. Also unwraps the @Data@ encoding.

@since 3.3.0
-}
unPPosixTime :: forall (s :: S). Term s PPosixTime -> Term s PInteger
unPPosixTime = pto
