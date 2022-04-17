{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Value (
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
) where

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )

import Plutarch.Prelude
import Plutarch.TryFrom (Flip, PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Plutarch.Unsafe (punsafeCoerce)

newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PTokenName PByteString)

instance PUnsafeLiftDecl PTokenName where type PLifted PTokenName = Plutus.TokenName
deriving via
  (DerivePConstantViaBuiltin Plutus.TokenName PTokenName PByteString)
  instance
    PConstantDecl Plutus.TokenName

newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PCurrencySymbol PByteString)

instance PUnsafeLiftDecl PCurrencySymbol where type PLifted PCurrencySymbol = Plutus.CurrencySymbol
deriving via
  (DerivePConstantViaBuiltin Plutus.CurrencySymbol PCurrencySymbol PByteString)
  instance
    PConstantDecl Plutus.CurrencySymbol

newtype PValue (s :: S) = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))

instance PUnsafeLiftDecl PValue where type PLifted PValue = Plutus.Value
deriving via
  (DerivePConstantViaNewtype Plutus.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  instance
    PConstantDecl Plutus.Value

instance PTryFrom PData (PAsData PTokenName) where
  type PTryFromExcess PData (PAsData PTokenName) = Flip Term PTokenName
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term _ (PAsData PByteString), unwrapped :: Term _ PByteString) <-
      tcont $ ptryFrom @(PAsData PByteString) opq
    tcont $ \f -> pif (pallBS # pisHexDigit # unwrapped) (f ()) (ptraceError "a TokenName must be hex-encoded")
    tcont $ \f -> pif (plengthBS # unwrapped #<= 32) (f ()) (ptraceError "a TokenName must be at most 32 Bytes long")
    pure (punsafeFromLifted wrapped, pcon . PTokenName $ unwrapped)
    where
      punsafeFromLifted :: PInner a b ~ c => Term s (PAsData c) -> Term s (PAsData a)
      punsafeFromLifted = punsafeCoerce

      pallBS :: Term s ((PInteger :--> PBool) :--> PByteString :--> PBool)
      pallBS = phoistAcyclic $
        plam $ \f str -> plet (plengthBS # str) $ \ln ->
          let helper :: Term _ (PInteger :--> PBool)
              helper = pfix #$ plam $ \self i ->
                pif
                  (i #< ln)
                  ( pif
                      (f #$ pindexBS # str # i)
                      (self # (i + 1))
                      (pcon PFalse)
                  )
                  (pcon PTrue)
           in helper # 0

      pisHexDigit :: Term s (PInteger :--> PBool)
      pisHexDigit = phoistAcyclic $
        plam $ \chr ->
          (chr #<= 57 #&& 48 #<= chr)
            #|| (chr #<= 70 #&& 65 #<= chr)
            #|| (chr #<= 102 #&& 97 #<= chr)

deriving via
  DerivePNewtype (PAsData PCurrencySymbol) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PCurrencySymbol)

deriving via
  DerivePNewtype (PAsData PValue) (PAsData (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  instance
    PTryFrom PData (PAsData PValue)
