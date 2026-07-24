{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module TH.MS (PTheseMS (..)) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.TH.Strategy (Strategy (MogensenScott), deriveFor)

data PTheseMS (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PThisMS (Term s a)
  | PThatMS (Term s b)
  | PTheseMS (Term s a) (Term s b)

deriveFor ''PTheseMS MogensenScott

{-
instance (PlutarchType a, PlutarchType b) => PlutarchType (PTheseMS a b) where
  type PRepresentation (PTheseMS a b) = PTheseMS a b

instance (PlutarchType a, PlutarchType b) => PMatch (PTheseMS a b) where
  pmatch' :: forall (c :: S -> Type) (s :: S) . Term s (PTheseMS a b) -> (PTheseMS a b s -> Term s c) -> Term s c
  pmatch' x f = let asMS = punsafeCoerce @_ @((a :--> c) :--> (b :--> c) :--> (a :--> b :--> c) :--> c) x
    in papp (papp (papp asMS (plam' $ \x -> f . PThisMS $ x)) (plam' $ \y -> f . PThatMS $ y)) (plam' $ \x -> plam' $ \y -> f . PTheseMS x $ y)

instance (PlutarchType a, PlutarchType b) => PCon (PTheseMS a b) where
  pcon' :: forall (s :: S) . PTheseMS a b s -> Term s (PTheseMS a b)
  pcon' = punsafeCoerce . \case
    PThisMS x -> plam' $ \f -> plam' $ \_ -> plam' $ \_ -> f # x
    PThatMS y -> plam' $ \_ -> plam' $ \f -> plam' $ \_ -> f # y
    PTheseMS x y -> plam' $ \_ -> plam' $ \_ -> plam' $ \f -> f # x # y

instance (PEq a, PEq b) => PEq (PTheseMS a b) where
  peq = plam' $ \x -> plam' $ \y ->
          let asMSX = punsafeCoerce @_ @((a :--> PBool) :--> (b :--> PBool) :--> (a :--> b :--> PBool) :--> PBool) x
              asMSY = punsafeCoerce @_ @((a :--> PBool) :--> (b :--> PBool) :--> (a :--> b :--> PBool) :--> PBool) y
            in asMSX # plam' (\x -> asMSY # plam' (\x' -> peq # x # x') # plam' (const pfalse) # plam' (\_ -> plam' $ const pfalse))
                     # plam' (\y -> asMSY # plam' (const pfalse) # plam' (\y' -> peq # y # y') # plam' (\_ -> plam' $ const pfalse))
                     # plam' (\x -> plam' $ \y -> asMSY # plam' (const pfalse) # plam' (const pfalse) # plam' (\x' -> plam' $ \y' -> pand (peq # x # x') (peq # y # y')))

-}
