{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Time (
  -- * Type
  PPosixTime (..),

  -- * Functions
  pposixTime,
  unPPosixTime,
) where

import GHC.Generics (Generic)
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 2.0.0
newtype PPosixTime (s :: S) = PPosixTime (Term s (PDataNewtype PInteger))
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
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since WIP
instance PCountable PPosixTime where
  {-# INLINEABLE psuccessor #-}
  psuccessor = phoistAcyclic $ plam (\x -> x #+ pposixTime pone)
  {-# INLINEABLE psuccessorN #-}
  psuccessorN = phoistAcyclic $ plam $ \p t ->
    let p' = pcon . PPosixTime . pcon . PDataNewtype . pdata . pto $ p
     in p' #+ t

-- | @since WIP
instance PEnumerable PPosixTime where
  {-# INLINEABLE ppredecessor #-}
  ppredecessor = phoistAcyclic $ plam (\x -> x #- pposixTime pone)
  {-# INLINEABLE ppredecessorN #-}
  ppredecessorN = phoistAcyclic $ plam $ \p t ->
    let p' = pcon . PPosixTime . pcon . PDataNewtype . pdata . pto $ p
     in t #- p'

-- | @since WIP
instance PAdditiveSemigroup PPosixTime where
  {-# INLINEABLE (#+) #-}
  t1 #+ t2 = pposixTime (unPPosixTime t1 #+ unPPosixTime t2)
  {-# INLINEABLE pscalePositive #-}
  pscalePositive t p = pposixTime (unPPosixTime t #* pto p)

-- | @since WIP
instance PAdditiveMonoid PPosixTime where
  {-# INLINEABLE pzero #-}
  pzero = pposixTime pzero
  {-# INLINEABLE pscaleNatural #-}
  pscaleNatural t n = pposixTime (unPPosixTime t #* pto n)

-- | @since WIP
instance PAdditiveGroup PPosixTime where
  {-# INLINEABLE pnegate #-}
  pnegate = phoistAcyclic $ plam $ \t -> pposixTime (pnegate # unPPosixTime t)
  {-# INLINEABLE (#-) #-}
  t1 #- t2 = pposixTime (unPPosixTime t1 #- unPPosixTime t2)

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PPosixTime Plutus.POSIXTime
  instance
    PLiftable PPosixTime

{- | Construct a 'PPosixTime' from a 'PInteger'. Same as using the constructor,
but a lot shorter.

@since WIP
-}
pposixTime :: forall (s :: S). Term s PInteger -> Term s PPosixTime
pposixTime = pcon . PPosixTime . pcon . PDataNewtype . pdata

{- | Unwrap a 'PPosixTime' to get a 'PInteger'. Same as using 'pmatch', but a
lot shorter. Also unwraps the @Data@ encoding.

@since WIP
-}
unPPosixTime :: forall (s :: S). Term s PPosixTime -> Term s PInteger
unPPosixTime t = pmatch t $ \(PPosixTime t') ->
  pmatch t' $ \(PDataNewtype t'') ->
    pfromData t''
