{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Time (
  -- * Type
  PPosixTime (..),

  -- * Functions
  pposixTime,
  unPPosixTime,
) where

import Plutarch.Builtin (PDataNewtype (PDataNewtype))
import Plutarch.Enum (
  PCountable (psuccessor, psuccessorN),
  PEnumerable (ppredecessor, ppredecessorN),
 )
import Plutarch.Internal.Lift (DeriveDataPLiftable)
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Num (PNum (pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-)))
import Plutarch.Prelude
import Plutarch.Reducible (Reduce)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since WIP
instance PCountable PPosixTime where
  {-# INLINEABLE psuccessor #-}
  psuccessor = phoistAcyclic $ plam (+ 1)
  {-# INLINEABLE psuccessorN #-}
  psuccessorN = phoistAcyclic $ plam $ \p t ->
    let p' = pcon . PPosixTime . pcon . PDataNewtype . pdata . pto $ p
     in p' + t

-- | @since WIP
instance PEnumerable PPosixTime where
  {-# INLINEABLE ppredecessor #-}
  ppredecessor = phoistAcyclic $ plam (\t -> t - 1)
  {-# INLINEABLE ppredecessorN #-}
  ppredecessorN = phoistAcyclic $ plam $ \p t ->
    let p' = pcon . PPosixTime . pcon . PDataNewtype . pdata . pto $ p
     in t - p'

-- | @since 2.0.0
instance PIntegral PPosixTime where
  {-# INLINEABLE pdiv #-}
  pdiv = phoistAcyclic $ plam $ \t1 t2 ->
    pposixTime (pdiv # unPPosixTime t1 # unPPosixTime t2)
  {-# INLINEABLE pmod #-}
  pmod = phoistAcyclic $ plam $ \t1 t2 ->
    pposixTime (pmod # unPPosixTime t1 # unPPosixTime t2)
  {-# INLINEABLE pquot #-}
  pquot = phoistAcyclic $ plam $ \t1 t2 ->
    pposixTime (pquot # unPPosixTime t1 # unPPosixTime t2)
  {-# INLINEABLE prem #-}
  prem = phoistAcyclic $ plam $ \t1 t2 ->
    pposixTime (prem # unPPosixTime t1 # unPPosixTime t2)

-- | @since 2.0.0
instance PNum PPosixTime where
  {-# INLINEABLE (#*) #-}
  t1 #* t2 = pposixTime (unPPosixTime t1 #* unPPosixTime t2)
  {-# INLINEABLE (#+) #-}
  t1 #+ t2 = pposixTime (unPPosixTime t1 #+ unPPosixTime t2)
  {-# INLINEABLE (#-) #-}
  t1 #- t2 = pposixTime (unPPosixTime t1 #- unPPosixTime t2)
  {-# INLINEABLE pabs #-}
  pabs = phoistAcyclic $ plam $ \t -> pposixTime (pabs # unPPosixTime t)
  {-# INLINEABLE pfromInteger #-}
  pfromInteger = pposixTime . pconstant
  {-# INLINEABLE pnegate #-}
  pnegate = phoistAcyclic $ plam $ \t -> pposixTime (pnegate # unPPosixTime t)
  {-# INLINEABLE psignum #-}
  psignum = phoistAcyclic $ plam $ \t -> pposixTime (psignum # unPPosixTime t)

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PPosixTime Plutus.POSIXTime
  instance
    PLiftable PPosixTime

-- | @since 3.1.0
instance PTryFrom PData PPosixTime where
  type PTryFromExcess PData PPosixTime = Mret PPosixTime
  ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    Term s PData ->
    ((Term s PPosixTime, Reduce (PTryFromExcess PData PPosixTime s)) -> Term s r) ->
    Term s r
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term s (PAsData PInteger), unwrapped :: Term s PInteger) <-
      tcont $ ptryFrom @(PAsData PInteger) opq
    pure (punsafeCoerce wrapped, pposixTime unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData PPosixTime) where
  type PTryFromExcess PData (PAsData PPosixTime) = Mret PPosixTime
  ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    Term s PData ->
    ((Term s (PAsData PPosixTime), Reduce (PTryFromExcess PData (PAsData PPosixTime) s)) -> Term s r) ->
    Term s r
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term s (PAsData PInteger), unwrapped :: Term s PInteger) <-
      tcont $ ptryFrom @(PAsData PInteger) opq
    pure (punsafeCoerce wrapped, pposixTime unwrapped)

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
