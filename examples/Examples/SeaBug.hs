{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.SeaBug () where

import Plutarch (ClosedTerm, PMatch)
import Plutarch.Prelude
import Plutarch.Unit
import Plutarch.Builtin
import Plutarch.Integer
import Plutarch.ByteString
import Plutarch.DataRepr
import Plutarch.Rec
import Plutarch.Rec.TH
import Plutarch.Pair
import qualified Plutarch.Monadic as P

import Plutarch.Api.V1
import Prelude

newtype Content s = Content {getContent :: PByteString s}

newtype PubKeyHashPH s = PubKeyHashPH {getPubKeyHashPH :: PByteString s}

type PNatural = PInteger

type PAssetClass = PPair PCurrencySymbol PTokenName

data NftId f = NftId
  { nftId'content :: f Content
  , nftId'collectionNft :: f PAssetClass
  , nftId'price :: f PNatural
  , nftId'owner :: f PubKeyHashPH
  , nftId'author :: f PubKeyHashPH
  , nftId'authorShare :: f PNatural
  , nftId'marketplaceValHash :: f PValidatorHash
  , nftId'marketplaceShare :: f PNatural
  }

deriveAll ''NftId

data MintAct (s :: S)
  = MintToken (Term s (PDataList '[PRecord NftId]))
  | ChangePrice (Term s (PDataList '[PRecord NftId, PNatural]))
  | ChangeOwner (Term s (PDataList '[PRecord NftId, PubKeyHashPH]))
  | BurnToken (Term s (PDataList '[PRecord NftId]))
  deriving
    ( PMatch
    , PIsData
    )
    via PIsDataReprInstances MintAct

instance PIsDataRepr MintAct where
  type
    PIsDataReprRepr MintAct =
      '[ '[PRecord NftId]
       , '[PRecord NftId, PNatural]
       , '[PRecord NftId, PubKeyHashPH]
       , '[PRecord NftId]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . MintToken) $
        DRHCons (f . ChangePrice) $
          DRHCons (f . ChangeOwner) $
            DRHCons (f . BurnToken) DRHNil

-- fieldFromData

-- checkMint nft =
--       let newName = mkTokenName nft
--        in case filter (\(cs, _, _) -> cs == ownCs) $ Value.flattenValue (txInfoMint info) of
--             [(_, tn, amt)] -> tn == newName && amt == 1
--             _ -> False

hash :: Term s (PRecord NftId :--> PByteString)
hash = _

checkMint :: Term s (PAsData NftId :--> PBool)
checkMint = plam $ \nftData -> P.do
  newName <- plet $ hash nftData



PRecord NftId


mkPolicy ::
  forall (s :: S).
  ClosedTerm PValidatorHash ->
  ClosedTerm (PMaybe PCurrencySymbol) ->
  ClosedTerm PAssetClass ->
  Term s (PAsData MintAct :--> PAsData PScriptContext :--> PUnit)
mkPolicy marketplaceValHash mCurrencySymbol ac =
  plam $
    \redeemer (ctx :: Term _ _) -> P.do
      mintAct <- pmatch $ pfromData redeemer
      case mintAct of
        MintToken ((pdhead #) -> nftId) -> perror
