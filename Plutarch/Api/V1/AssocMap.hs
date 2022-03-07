{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.AssocMap (
  PMap,
  pmkPMap,
) where

import Plutarch.TryFrom (
  PMaybeFrom (PMaybeFromExcess, pmaybeFrom),
  PTryFrom (PTryFromExcess, ptryFrom),
 )
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as PlutusMap

import Plutarch.Builtin (PBuiltinMap)
import Plutarch.Lift (
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.Prelude

newtype PMap (k :: PType) (v :: PType) (s :: S) = PMap (Term s (PBuiltinMap k v))
  deriving (PlutusType, PIsData) via (DerivePNewtype (PMap k v) (PBuiltinMap k v))

instance
  ( Plutus.ToData (PLifted v)
  , Plutus.ToData (PLifted k)
  , Plutus.FromData (PLifted v)
  , Plutus.FromData (PLifted k)
  , PLift k
  , PLift v
  ) =>
  PUnsafeLiftDecl (PMap k v)
  where
  type PLifted (PMap k v) = PlutusMap.Map (PLifted k) (PLifted v)

instance
  ( PLifted (PConstanted k) ~ k
  , Plutus.ToData v
  , Plutus.FromData v
  , Plutus.ToData k
  , Plutus.FromData k
  , PConstant k
  , PLifted (PConstanted v) ~ v
  , Plutus.FromData v
  , Plutus.ToData v
  , PConstant v
  ) =>
  PConstant (PlutusMap.Map k v)
  where
  type PConstantRepr (PlutusMap.Map k v) = [(Plutus.Data, Plutus.Data)]
  type PConstanted (PlutusMap.Map k v) = PMap (PConstanted k) (PConstanted v)
  pconstantToRepr m = (\(x, y) -> (Plutus.toData x, Plutus.toData y)) <$> PlutusMap.toList m
  pconstantFromRepr m = fmap PlutusMap.fromList $
    flip traverse m $ \(x, y) -> do
      x' <- Plutus.fromData x
      y' <- Plutus.fromData y
      Just (x', y')

----------------------- Smart constructor to to create onchain instances ----------------

{- |
    Smart constructor to enforce PMap invariants on the Haskell level for writing
    onchain code
-}
pmkPMap ::
  forall a b s.
  ( PLifted (PConstanted a) ~ a
  , PLifted (PConstanted b) ~ b
  , PLift (PConstanted a)
  , PLift (PConstanted b)
  , Plutus.ToData a
  , Plutus.ToData b
  , Plutus.FromData a
  , Plutus.FromData b
  , Ord a
  ) =>
  [(a, b)] ->
  Maybe (Term s (PMap (PConstanted a) (PConstanted b)))
pmkPMap l = if sorted l then Just (pconstant $ PlutusMap.fromList l) else Nothing
  where
    sorted [] = True
    sorted [_] = True
    sorted (x : y : zs) = if fst x > fst y then False else sorted (y : zs)

----------------------- PTryFrom and PMaybeFrom instances -------------------------------

instance
  ( POrd k
  , PIsData k
  ) =>
  PTryFrom (PBuiltinMap k v) (PMap k v)
  where
  type PTryFromExcess (PBuiltinMap k v) (PMap k v) = PUnit
  ptryFrom oMap = runTermCont $ do
    sortVer <-
      tcont $
        plet $
          ( pfix #$ plam $
              \self xs ->
                pmatch xs $ \case
                  PNil -> pcon PUnit
                  PCons x ys ->
                    pmatch ys $ \case
                      PNil -> pcon PUnit
                      PCons y _ ->
                        pif
                          ((pfromData (pfstBuiltin # x)) #< (pfromData (pfstBuiltin # y)))
                          (self # ys)
                          perror
          )
            # oMap
    pure ((pcon . PMap) oMap, sortVer)

instance
  ( POrd k
  , PIsData k
  ) =>
  PMaybeFrom (PBuiltinMap k v) (PMap k v)
  where
  type PMaybeFromExcess (PBuiltinMap k v) (PMap k v) = PUnit
  pmaybeFrom oMap = runTermCont $ do
    sortVer <-
      tcont $
        plet $
          ( pfix #$ plam $
              \self xs ->
                pmatch xs $ \case
                  PNil -> pcon $ PJust $ pcon PUnit
                  PCons x ys ->
                    pmatch ys $ \case
                      PNil -> pcon $ PJust $ pcon PUnit
                      PCons y _ ->
                        pif
                          ((pfromData (pfstBuiltin # x)) #< (pfromData (pfstBuiltin # y)))
                          (self # ys)
                          (pcon PNothing)
          )
            # oMap
    ver <- tcont $ pmatch sortVer
    pure $ case ver of
      PJust _ -> ((pcon . PJust . pcon . PMap) oMap, sortVer)
      PNothing -> (pcon PNothing, sortVer)
