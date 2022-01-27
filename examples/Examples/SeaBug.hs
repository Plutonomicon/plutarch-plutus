{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Examples.SeaBug (mkPolicy) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Control.Monad.Trans.Cont (cont, runCont)
import Plutarch (ClosedTerm)
import Plutarch.Prelude
import Plutarch.DataRepr
import qualified Plutarch.Monadic as P

import Plutarch.Api.V1 hiding (PMaybe)
import Prelude

type PNatural = PInteger

data NftId (s :: S) =
  NftId (Term s (PDataRecord
   '[ "collectionNftTn" ':= PTokenName
    , "price" ':= PNatural
    , "owner" ':= PPubKeyHash
    ]
  ))
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances NftId
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass PIsDataRepr

data MintAct (s :: S)
  = MintToken (Term s (PDataRecord '[ "nftId" ':= NftId]))
  | ChangePrice (Term s (PDataRecord '[ "nftId" ':= NftId, "price" ':= PNatural]))
  | ChangeOwner (Term s (PDataRecord '[ "nftId" ':= NftId, "owner" ':= PPubKeyHash]))
  | BurnToken (Term s (PDataRecord '[ "nftId" ':= NftId]))
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances MintAct
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass PIsDataRepr

-- fieldFromData

-- checkMint nft =
--       let newName = mkTokenName nft
--        in case filter (\(cs, _, _) -> cs == ownCs) $ Value.flattenValue (txInfoMint info) of
--             [(_, tn, amt)] -> tn == newName && amt == 1
--             _ -> False

hash :: Term s (NftId :--> PByteString)
hash = perror

-- checkMint nft =
--   let newName = mkTokenName nft
--    in case filter (\(cs, _, _) -> cs == ownCs) $ Value.flattenValue mintedValue of
--         [(_, tn, amt)] -> tn == newName && amt == 1
--         _ -> False

-- passert :: Term s (PBool :--> PUnit)
-- passert = phoistAcyclic $ plam $ \b -> pif b (pcon PUnit) perror

passert :: forall (s :: S) (a :: PType). Term s PBool -> Term s a -> Term s a
passert b inp = pif b inp perror

passert' :: forall (s :: S) (a :: PType). Term s PBool -> (() -> Term s a) -> Term s a
passert' b inp = pif b (inp ()) perror

checkMint :: Term s (PCurrencySymbol :--> NftId :--> PValue :--> PUnit)
checkMint = plam $ \ownCS nftData mintedValue -> P.do
  newName <- plet $ hash # nftData
  csMap <- plet $ pto $ pto mintedValue
  filteredCSMap <- plet $ pfilter # (plam $ \(pfromData . (pfstBuiltin #) -> cs) -> cs #== ownCS) # csMap
  tokenNameMap <- plet $ pfromData $ psndBuiltin #$ phead # filteredCSMap
  passert $ plength # pto tokenNameMap #== 1
  pair <- plet $ phead # pto tokenNameMap

  passert $ pfstBuiltin # pair #== pdata (pcon $ PTokenName newName)
  passert $ psndBuiltin # pair #== pdata 1
  pcon PUnit

checkMintCont :: Term s (PCurrencySymbol :--> NftId :--> PValue :--> PUnit)
checkMintCont = plam $ \ownCS nftData mintedValue -> (`runCont` id) $ do
  newName <- cont $ plet $ hash # nftData
  csMap <- cont $ plet $ pto $ pto mintedValue
  filteredCSMap <- cont $ plet $ pfilter # (plam $ \(pfromData . (pfstBuiltin #) -> cs) -> cs #== ownCS) # csMap
  tokenNameMap <- cont $ plet $ pfromData $ psndBuiltin #$ phead # filteredCSMap
  cont $ passert' $ plength # pto tokenNameMap #== 1
  pair <- cont $ plet $ phead # pto tokenNameMap

  cont $ passert' $ pfstBuiltin # pair #== pdata (pcon $ PTokenName newName)
  cont $ passert' $ psndBuiltin # pair #== pdata 1
  cont $ const $ pcon PUnit

-- PRecord NftId

{-
    MintToken nft ->
      traceIfFalse "Exactly one NFT must be minted" (checkMint nft)
        && traceIfFalse "Collection NFT must be burned" (checkCollectionNftBurned nft)

    ownCs = ownCurrencySymbol ctx

    checkCollectionNftBurned nft =
      let lockingAddress = scriptHashAddress lockingScript
          containsCollectonNft tx =
            txOutAddress tx == lockingAddress
              && Value.valueOf (txOutValue tx) collectionNftCs (nftId'collectionNftTn nft) == 1
       in any containsCollectonNft (txInfoOutputs info)
-}


mkPolicy ::
  forall (s :: S).
  ClosedTerm PCurrencySymbol ->
  ClosedTerm PValidatorHash ->
  ClosedTerm PPubKeyHash ->
  ClosedTerm PNatural ->
  ClosedTerm PValidatorHash ->
  ClosedTerm PNatural ->
  Term s (PAsData MintAct :--> PAsData PScriptContext :--> PUnit)
mkPolicy collectionNftCs lockingScript author authorShare marketplaceScript marketplaceShare =
  plam $
    \redeemer ctx ->
      pmatch (pfromData redeemer) $ \case
        MintToken nftId -> P.do
          fs <- pletFields @'["txInfo", "purpose"] $ pfromData ctx
          (pfromData -> mintValue) <- plet $ pfield @"mint" #$ pfromData fs.txInfo

          (PMinting mintPurposeRecord) <- pmatch $ pfromData fs.purpose
          (pfromData -> ownCS) <- plet $ pfield @"_0" # mintPurposeRecord

          (pfromData -> nftRecord) <- plet $ pfield @"nftId" # nftId
          
          _ <- plet $ checkMintCont # ownCS # nftRecord # mintValue
          checkMint # ownCS # nftRecord # mintValue
