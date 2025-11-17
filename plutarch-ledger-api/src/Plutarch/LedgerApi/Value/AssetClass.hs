module Plutarch.LedgerApi.Value.AssetClass (
  PAssetClass (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol)
import Plutarch.LedgerApi.Value.TokenName (PTokenName)
import Plutarch.Prelude
import PlutusLedgerApi.V1.Value qualified as PlutusValue

-- | @since 3.3.0
newtype PAssetClass (s :: S) = PAssetClass (Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName)))
  deriving stock
    ( -- | @since 3.3.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.3.0
      PIsData
    , -- | @since 3.3.0
      PEq
    , -- | @since 3.3.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PAssetClass)

-- | @since 3.3.0
instance POrd PAssetClass where
  {-# INLINEABLE (#<=) #-}
  ac1 #<= ac2 = pmatch ac1 $ \(PAssetClass pair1) ->
    pmatch ac2 $ \(PAssetClass pair2) ->
      pmatch pair1 $ \(PBuiltinPair fst1' snd1') ->
        pmatch pair2 $ \(PBuiltinPair fst2' snd2') ->
          plet (pfromData fst1') $ \fst1 ->
            plet (pfromData fst2') $ \fst2 ->
              (fst1 #< fst2)
                #|| ( (fst1 #== fst2)
                        #&& ( let snd1 = pfromData snd1'
                                  snd2 = pfromData snd2'
                               in snd1 #<= snd2
                            )
                    )
  {-# INLINEABLE (#<) #-}
  ac1 #< ac2 = pmatch ac1 $ \(PAssetClass pair1) ->
    pmatch ac2 $ \(PAssetClass pair2) ->
      pmatch pair1 $ \(PBuiltinPair fst1' snd1') ->
        pmatch pair2 $ \(PBuiltinPair fst2' snd2') ->
          plet (pfromData fst1') $ \fst1 ->
            plet (pfromData fst2') $ \fst2 ->
              (fst1 #< fst2)
                #|| ( (fst1 #== fst2)
                        #&& ( let snd1 = pfromData snd1'
                                  snd2 = pfromData snd2'
                               in snd1 #< snd2
                            )
                    )

-- | @since 3.3.0
deriving via
  DeriveNewtypePLiftable PAssetClass PlutusValue.AssetClass
  instance
    PLiftable PAssetClass

-- | @since 3.4.0
instance PTryFrom PData (PAsData PAssetClass)
