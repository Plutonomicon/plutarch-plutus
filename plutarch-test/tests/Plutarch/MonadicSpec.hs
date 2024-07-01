{-# LANGUAGE QualifiedDo #-}

module Plutarch.MonadicSpec (spec) where

import Control.Monad.Trans.Cont (cont, runCont)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1 (
  PAddress (PAddress),
  PCredential,
  PStakingCredential,
 )
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "monadic" $ do
    describe "pmatch-twice" . pgoldenSpec $ do
      -- We expect all these benchmarks to produce equivalent numbers
      let integerList :: [Integer] -> Term s (PList PInteger)
          integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
          xs = integerList [1 .. 10]
      "normal"
        @| pmatch xs
        $ \case
          PSCons _x xs' -> do
            pmatch xs' $ \case
              PSCons _ xs'' ->
                xs''
              PSNil -> perror
          PSNil -> perror
      "do"
        @| P.do
          PSCons _ xs' <- pmatch xs
          PSCons _ xs'' <- pmatch xs'
          xs''
      "cont"
        @| flip runCont id
        $ do
          ls <- cont $ pmatch xs
          case ls of
            PSCons _ xs' -> do
              ls' <- cont $ pmatch xs'
              case ls' of
                PSCons _ xs'' -> pure xs''
                PSNil -> pure perror
            PSNil -> pure perror
      "termcont"
        @| unTermCont
        $ do
          PSCons _ xs' <- TermCont $ pmatch xs
          PSCons _ xs'' <- TermCont $ pmatch xs'
          pure xs''
    describe "api.example" $ do
      describe "getFields" . pgoldenSpec $ do
        "0" @| getFields

getFields :: Term s (PAddress :--> PDataRecord '["credential" ':= PCredential, "stakingCredential" ':= PMaybeData PStakingCredential])
getFields = phoistAcyclic $
  plam $ \addr -> P.do
    PAddress addrFields <- pmatch addr
    addrFields
