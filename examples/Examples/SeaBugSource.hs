data NftMintingParams = NftMintingParams
  { nftConfigCurrencySymbol :: CurrencySymbol
  , nftValidatorAddress :: ValidatorHash
  }

txOutputFilter :: ValidatorHash -> Term s (PData :--> PBool)
txOutputFilter (ValidatorHash vhbs) = phoistAcyclic $ plam $ \inData ->
  plet (phead #$ psndBuiltin #$ pasConstr # inData) $ \addr ->
    plet (pasConstr #$ phead #$ psndBuiltin #$ pasConstr # addr) $ \credConstPair ->
      pif (pfstBuiltin # credConstPair #== 0) (pcon PFalse) $
        (punsafeCoerce $ phead #$ psndBuiltin # credConstPair) #== pconstant @PByteString (fromBuiltin vhbs)
        

ptxInInfoResolved :: Term s (PData :--> PData)
ptxInInfoResolved = phoistAcyclic $ plam $ \txInfoData ->
  phead #$ ptail #$ psndBuiltin #$ pasConstr # txInfoData

mintingPolicy :: NftMintingParams -> Term s (PData :--> PAsData PScriptContext :--> PUnit)
mintingPolicy params = plam $ \_ ctx ->
  pmatch (pfromData ctx) $ \(PScriptContext scriptContext) ->
    pmatch (pfromData (pdhead # scriptContext)) $ \(PTxInfo txInfo) ->
      plet (pdhead # txInfo) $ \inputs ->
        plet (pdhead #$ pdtail # txInfo) $ \outputs ->
          pif 
            ( (pany # txOutputFilter (nftValidatorAddress params) # pfromData outputs) 
                #&& (pany # txOutputFilter (nftValidatorAddress params) #$ pmap # ptxInInfoResolved # pfromData inputs)
            )
            (pcon PUnit)
            perror