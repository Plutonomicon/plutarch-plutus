{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Time (
  PPosixTime (..),
) where

import Plutarch.Builtin (PDataNewtype (PDataNewtype))
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
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

-- | @since 2.0.0
instance PIntegral PPosixTime where
  pdiv = phoistAcyclic $ plam $ \t1 t2 ->
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            pcon . PPosixTime . pcon . PDataNewtype . pdata $ pdiv # pfromData t1'' # pfromData t2''
  pmod = phoistAcyclic $ plam $ \t1 t2 ->
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            pcon . PPosixTime . pcon . PDataNewtype . pdata $ pmod # pfromData t1'' # pfromData t2''
  pquot = phoistAcyclic $ plam $ \t1 t2 ->
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            pcon . PPosixTime . pcon . PDataNewtype . pdata $ pquot # pfromData t1'' # pfromData t2''
  prem = phoistAcyclic $ plam $ \t1 t2 ->
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            pcon . PPosixTime . pcon . PDataNewtype . pdata $ pquot # pfromData t1'' # pfromData t2''

-- | @since 2.0.0
instance PNum PPosixTime where
  t1 #* t2 =
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            pcon . PPosixTime . pcon . PDataNewtype . pdata $ pfromData t1'' #+ pfromData t2''
  t1 #+ t2 =
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            pcon . PPosixTime . pcon . PDataNewtype . pdata $ pfromData t1'' #+ pfromData t2''
  t1 #- t2 =
    pmatch t1 $ \(PPosixTime t1') ->
      pmatch t2 $ \(PPosixTime t2') ->
        pmatch t1' $ \(PDataNewtype t1'') ->
          pmatch t2' $ \(PDataNewtype t2'') ->
            plet (pfromData t1'' #- pfromData t2'') $ \res ->
              pif
                (0 #<= res)
                (pcon . PPosixTime . pcon . PDataNewtype . pdata $ res)
                (ptraceInfoError "PPosixTime subtraction gave a negative value")
  pabs = phoistAcyclic $ plam id
  pfromInteger i
    | i < 0 = ptraceInfoError "Cannot make PPosixTime from a negative value"
    | otherwise = pcon . PPosixTime . pcon . PDataNewtype . pdata . pconstant $ i
  pnegate = phoistAcyclic $ plam $ \_ -> ptraceInfoError "PPosixTime can't be negative"
  psignum = phoistAcyclic $ plam $ \t ->
    pmatch t $ \(PPosixTime t') ->
      pmatch t' $ \(PDataNewtype t'') ->
        pcon . PPosixTime . pcon . PDataNewtype . pdata $
          pif
            (pfromData t'' #== 0)
            0
            1

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PPosixTime where
  type PLifted PPosixTime = Plutus.POSIXTime

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.POSIXTime PPosixTime)
  instance
    PConstantDecl Plutus.POSIXTime

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
    tcont $ \f -> pif (0 #<= unwrapped) (f ()) (ptraceInfoError "ptryFrom(POSIXTime): must be positive")
    pure (punsafeCoerce wrapped, pcon . PPosixTime . pcon . PDataNewtype . pdata $ unwrapped)

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
    tcont $ \f -> pif (0 #<= unwrapped) (f ()) (ptraceInfoError "ptryFrom(POSIXTime): must be positive")
    pure (punsafeCoerce wrapped, pcon . PPosixTime . pcon . PDataNewtype . pdata $ unwrapped)
