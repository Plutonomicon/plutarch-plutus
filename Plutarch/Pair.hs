module Plutarch.Pair (PPair (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (PType, PlutusType, S, Term,(#),pmatch,plam,phoistAcyclic)
import Plutarch.Bool (PEq(..),POrd(..),(#&&),(#||))

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)


instance (PEq a, PEq b) => PEq (PPair a b) where
  a' #== b' =
    phoistAcyclic
      ( plam $ \a b ->
          pmatch a $ \(PPair al ar) ->
            pmatch b $ \(PPair bl br) ->
              al #== bl #&& ar #== br
      )
      # a'
      # b'

instance (PEq a, POrd a, POrd b) => POrd (PPair a b) where
  a' #< b' =
    phoistAcyclic
      ( plam $ \a b ->
          pmatch a $ \(PPair al ar) ->
            pmatch b $ \(PPair bl br) ->
              (al #< bl) #|| ((al #== bl) #&& (ar #< br))
      )
      # a'
      # b'

  a' #<= b' =
    phoistAcyclic
      ( plam $ \a b ->
          pmatch a $ \(PPair al ar) ->
            pmatch b $ \(PPair bl br) ->
              (al #< bl) #|| ((al #== bl) #&& (ar #<= br))
      )
      # a'
      # b'

