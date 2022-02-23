module Plutarch.Extra.List (
  psort,
  mergeSort,
  timSort,
) where

import Plutarch.Prelude

import Plutarch.Extra.Monadic (tmatch)

psort :: POrd a => Term s (PList a :--> PList a)
psort = timSort

-- TODO: decide on default sort
-- probably by benchmarking them

mergeSort :: POrd a => Term s (PList a :--> PList a)
mergeSort = phoistAcyclic $
  pfix #$ plam $ \self xs ->
    pmatch xs $ \case
      PSNil -> pcon PSNil
      PSCons x t ->
        pmatch t $ \case
          PSNil -> psingleton # x
          PSCons _ _ -> unTermCont $ do
            PPair l r <- tmatch $ splitList # xs
            pure $
              merge # (self # l) # (self # r)

splitList :: Term s (PList a :--> PPair (PList a) (PList a))
splitList = phoistAcyclic $
  pfix #$ plam $ \self xs -> pmatch xs $ \case
    PSNil -> pcon $ PPair pnil pnil
    PSCons x xs' ->
      pmatch xs' $ \case
        PSNil -> pcon $ PPair (psingleton # x) pnil
        PSCons y ys -> unTermCont $ do
          PPair l r <- tmatch $ self # ys
          pure $
            pcon $ PPair (pcons # x # l) (pcons # y # r)

merge :: POrd a => Term s (PList a :--> PList a :--> PList a)
merge = phoistAcyclic $
  pfix #$ plam $ \self xs ys ->
    pmatch xs $ \case
      PSNil -> ys
      PSCons x xs' ->
        pmatch ys $ \case
          PSNil -> xs
          PSCons y ys' ->
            pif
              (x #<= y)
              (pcons # x #$ self # xs' # ys)
              (pcons # y #$ self # xs # ys')

-- most of the tricks from timsort are not implemented here
-- just the central strategy of finding runs of
-- consecutive data and then merging them together
timSort :: POrd a => Term s (PList a :--> PList a)
timSort = phoistAcyclic $
  plam $ \xs -> merge2 #$ timSplit # xs

timSplit :: POrd a => Term s (PList a :--> PList (PList a))
timSplit = phoistAcyclic $ timSplit' # pcon PTrue # pnil

timSplit' :: POrd a => Term s (PBool :--> PList a :--> PList a :--> PList (PList a))
timSplit' = phoistAcyclic $
  pfix #$ plam $ \self increasing state xs ->
    pmatch state $ \case
      PSNil -> pmatch xs $ \case
        PSCons x xs' -> self # increasing # (psingleton # x) # xs'
        PSNil -> pnil
      PSCons y _ ->
        pmatch xs $ \case
          PSNil ->
            pif
              increasing
              (psingleton #$ preverse # state)
              (psingleton # state)
          PSCons x xs' ->
            pif
              ((increasing #&& (y #<= x)) #|| ((pnot # increasing) #&& (x #<= y)))
              (self # increasing # (pcons # x # state) # xs')
              ( pcons
                  # pif increasing (preverse # state) state
                  # (self # (pnot # increasing) # (psingleton # x) # xs')
              )

merge2 :: POrd a => Term s (PList (PList a) :--> PList a)
merge2 = phoistAcyclic $
  pfix #$ plam $ \self xss ->
    pmatch xss $ \case
      PSNil -> pnil
      PSCons xs xss' ->
        pmatch xss' $ \case
          PSNil -> xs
          PSCons _ _ ->
            self #$ merge2' # xss

merge2' :: POrd a => Term s (PList (PList a) :--> PList (PList a))
merge2' = phoistAcyclic $
  pfix #$ plam $ \self xss ->
    pmatch xss $ \case
      PSNil -> pnil
      PSCons xs1 xss' ->
        pmatch xss' $ \case
          PSNil -> psingleton # xs1
          PSCons xs2 xss'' ->
            pcons # (merge # xs1 # xs2) #$ self # xss''
