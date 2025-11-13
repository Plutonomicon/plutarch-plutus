{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Show (
  PShow (pshow'),
  pshow,
  pshowAndErr,
  pshowList,
) where

import Data.Char (intToDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (sconcat)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Generics.SOP (
  All,
  All2,
  ConstructorName,
  K (K),
  NP,
  NS,
  Proxy (Proxy),
  SOP (SOP),
  constructorInfo,
  constructorName,
  hcmap,
  hcollapse,
  hindex,
  hmap,
 )
import Generics.SOP.GGP (gdatatypeInfo)
import Plutarch.Builtin.Bool (PBool, pif)
import Plutarch.Builtin.ByteString (
  PByte,
  PByteString,
  pbyteToInteger,
  pconsBS,
  pindexBS,
  pintegerToByte,
  plengthBS,
  psliceBS,
 )
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PBuiltinPair,
  PData,
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pchooseData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.String (
  PString,
  pdecodeUtf8,
  pencodeUtf8,
 )
import Plutarch.Builtin.Unit (PUnit)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Fix (pfixHoisted)
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom)
import Plutarch.Internal.IsData (PIsData, pfromData)
import Plutarch.Internal.Lift (PlutusRepr, pconstant)
import Plutarch.Internal.ListLike (
  PIsListLike,
  PListLike (pelimList),
  pfoldr',
  pmap,
  precList,
 )
import Plutarch.Internal.Numeric (PPositive, pquot, prem)
import Plutarch.Internal.Ord (POrd ((#<)))
import Plutarch.Internal.PLam (PLamN (plam))
import Plutarch.Internal.PlutusType (PlutusType, pmatch)
import Plutarch.Internal.Term (
  Term,
  pdelay,
  perror,
  pforce,
  phoistAcyclic,
  plet,
  punsafeCoerce,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Maybe (PMaybe)

import PlutusCore qualified as PLC

class PShow t where
  {- | Return the string representation of a Plutarch value

  If the wrap argument is True, optionally wrap the output in `(..)` if it
  represents multiple parameters.
  -}
  pshow' :: Bool -> Term s t -> Term s PString
  default pshow' :: (PGeneric t, PlutusType t, All2 PShow (PCode t)) => Bool -> Term s t -> Term s PString
  pshow' wrap x = gpshow wrap # x

pshowList :: forall list a s. (PShow a, PIsListLike list a) => Term s (list a :--> PString)
pshowList =
  phoistAcyclic $
    plam $ \list ->
      "[" <> pshowList' @list @a # list <> "]"

pshowList' :: forall list a s. (PShow a, PIsListLike list a) => Term s (list a :--> PString)
pshowList' =
  phoistAcyclic $
    precList
      ( \self x xs ->
          pelimList
            (\_ _ -> pshow x <> ", " <> self # xs)
            (pshow x)
            xs
      )
      (const "")

-- | Return the string representation of a Plutarch value
pshow :: PShow a => Term s a -> Term s PString
pshow = pshow' False

-- | Case matching on bytestring, as if a list.
pelimBS ::
  Term
    s
    ( PByteString
        :--> a -- If bytestring is empty
        :--> (PByte :--> PByteString :--> a) -- If bytestring is non-empty
        :--> a
    )
pelimBS = phoistAcyclic $
  plam $ \bs z f ->
    plet (plengthBS # bs) $ \n ->
      pif (n #== 0) z $
        plet (pindexBS # bs # 0) $ \x ->
          plet (psliceBS # 1 # (n - 1) # bs) $ \xs ->
            f # x # xs

pcase :: PEq a => Term s b -> Term s a -> [(Term s a, Term s b)] -> Term s b
pcase y x = \case
  [] -> y
  ((x', r) : cs) -> pif (x #== x') r $ pcase y x cs

-- | Generic version of `pshow`
gpshow ::
  forall a s.
  (PGeneric a, PlutusType a, All2 PShow (PCode a)) =>
  Bool ->
  Term s (a :--> PString)
gpshow wrap =
  let constructorNames :: [ConstructorName] =
        hcollapse $ hmap (K . constructorName) $ constructorInfo $ gdatatypeInfo (Proxy @(a s))
   in phoistAcyclic $
        plam $ \x ->
          pmatch x $ \x' ->
            productGroup wrap " " $ gpshow' constructorNames (gpfrom x')

-- | Like `gpshow`, but returns the individual parameters list
gpshow' ::
  forall a s.
  All2 PShow a =>
  [ConstructorName] ->
  SOP (Term s) a ->
  NonEmpty (Term s PString)
gpshow' constructorNames (SOP x) =
  let cName = constructorNames !! hindex x
   in pconstant @PString (T.pack cName) :| showSum x
  where
    showSum :: NS (NP (Term s)) a -> [Term s PString]
    showSum =
      hcollapse . hcmap (Proxy @(All PShow)) showProd
    showProd :: All PShow xs => NP (Term s) xs -> K [Term s PString] xs
    showProd =
      K . hcollapse . hcmap (Proxy @PShow) showTerm
    showTerm :: forall b. PShow b => Term s b -> K (Term s PString) b
    showTerm =
      K . pshow' True

-- | Group parameters list, preparing for final PShow output
productGroup :: (Monoid a, IsString a) => Bool -> a -> NonEmpty a -> a
productGroup wrap sep = \case
  x :| [] -> x
  xs ->
    let xs' = sconcat $ NE.intersperse sep xs
     in if wrap then fromString "(" <> xs' <> fromString ")" else xs'

{- | Causes an error where the input is shown in the message.
 Works for all types.
-}
pshowAndErr :: Term s a -> Term s b
pshowAndErr x = punsafeCoerce $ pindexBS # punsafeCoerce (pif (punsafeCoerce x) x x) # 0

--------------------------------------------------------------------------------

instance PShow PUnit where
  pshow' _ _ = "()"

instance PShow PString where
  pshow' _ x = pshowStr # x
    where
      pshowStr :: Term s (PString :--> PString)
      pshowStr = phoistAcyclic $
        plam $ \s ->
          "\"" <> (pdecodeUtf8 #$ pshowUtf8Bytes #$ pencodeUtf8 # s) <> "\""
      pshowUtf8Bytes :: Term s (PByteString :--> PByteString)
      pshowUtf8Bytes = phoistAcyclic $
        pfixHoisted #$ plam $ \self bs ->
          pelimBS
            # bs
            # bs
            #$ plam
            $ \x xs ->
              -- Non-ascii byte sequence will not use bytes < 128.
              -- So we are safe to rewrite the lower byte values.
              -- https://en.wikipedia.org/wiki/UTF-8#Encoding
              let doubleQuote :: Term _ PInteger = 34 -- `"`
                  escapeSlash :: Term _ PInteger = 92 -- `\`
                  rec_ = pconsBS # x #$ self # xs
               in pif
                    (x #== (pintegerToByte # doubleQuote))
                    (pconsBS # (pintegerToByte # escapeSlash) # rec_)
                    rec_

instance PShow PBool where
  pshow' _ x = pshowBool # x
    where
      pshowBool :: Term s (PBool :--> PString)
      pshowBool = phoistAcyclic $
        plam $ \x ->
          -- Delegate to Haskell's Show instance
          pmatch x $ pconstant @PString . T.pack . show

instance PShow PInteger where
  pshow' _ x = pshowInt # x
    where
      pshowInt :: Term s (PInteger :--> PString)
      pshowInt = phoistAcyclic $
        pfixHoisted #$ plam $ \self n ->
          let sign = pif (n #< 0) "-" ""
           in sign
                <> plet
                  (pquot # abs n # 10)
                  ( \q ->
                      plet (prem # abs n # 10) $ \r ->
                        pif
                          (q #== 0)
                          (pshowDigit # r)
                          ( plet (self # q) $ \prefix ->
                              prefix <> pshowDigit # r
                          )
                  )
      pshowDigit :: Term s (PInteger :--> PString)
      pshowDigit = phoistAcyclic $
        plam $ \digit ->
          pcase perror digit $
            flip fmap [0 .. 9] $ \(x :: Integer) ->
              (pconstant x, pconstant (T.pack . show $ x))

instance PShow PByteString where
  pshow' _ x = showByteString # x
    where
      showByteString :: Term s (PByteString :--> PString)
      showByteString = phoistAcyclic $
        plam $ \bs ->
          "0x" <> showByteString' # bs
      showByteString' :: Term s (PByteString :--> PString)
      showByteString' = phoistAcyclic $
        pfixHoisted #$ plam $ \self bs ->
          pelimBS
            # bs
            # pconstant @PString ""
            #$ plam
            $ \x xs -> showByte # x <> self # xs
      showByte :: Term s (PByte :--> PString)
      showByte = phoistAcyclic $
        plam $ \n' ->
          plet (pbyteToInteger # n') $ \n ->
            plet (pquot # n # 16) $ \a ->
              plet (prem # n # 16) $ \b ->
                showNibble # a <> showNibble # b
      showNibble :: Term s (PInteger :--> PString)
      showNibble = phoistAcyclic $
        plam $ \n ->
          pcase perror n $
            flip fmap [0 .. 15] $ \(x :: Int) ->
              ( pconstant $ toInteger x
              , pconstant @PString $ T.pack [intToDigit x]
              )

instance PShow PData where
  pshow' b t0 = wrap (go0 # t0)
    where
      wrap s = pif (pconstant b) ("(" <> s <> ")") s
      go0 :: Term s (PData :--> PString)
      go0 = phoistAcyclic $
        pfixHoisted #$ plam $ \go t ->
          let pshowConstr pp0 = plet pp0 $ \pp ->
                "Constr "
                  <> pshow' False (pfstBuiltin # pp)
                  <> " "
                  <> pshowListPString # (pmap # go # (psndBuiltin # pp))
              pshowMap pplist =
                "Map " <> pshowListPString # (pmap # pshowPair # pplist)
              pshowPair = plam $ \pp0 -> plet pp0 $ \pp ->
                "("
                  <> (go # (pfstBuiltin # pp))
                  <> ", "
                  <> (go # (psndBuiltin # pp))
                  <> ")"
              pshowList xs = "List " <> pshowListPString # (pmap # go # xs)
              pshowListPString = phoistAcyclic $
                plam $ \plist ->
                  "["
                    <> pelimList
                      ( \x0 xs0 ->
                          x0 <> (pfoldr' (\x r -> ", " <> x <> r) # ("" :: Term s PString) # xs0)
                      )
                      ""
                      plist
                    <> "]"
           in pforce $
                pchooseData
                  # t
                  # pdelay (pshowConstr (pasConstr # t))
                  # pdelay (pshowMap (pasMap # t))
                  # pdelay (pshowList (pasList # t))
                  # pdelay ("I " <> pshow (pasInt # t))
                  # pdelay ("B " <> pshow (pasByteStr # t))

instance (PIsData a, PShow a) => PShow (PAsData a) where
  pshow' w x = pshow' w (pfromData x)

instance
  ( PShow a
  , PLC.Contains PLC.DefaultUni (PlutusRepr a)
  ) =>
  PShow (PBuiltinList a)
  where
  pshow' _ x = pshowList @PBuiltinList @a # x

instance (PShow a, PShow b) => PShow (PBuiltinPair a b) where
  pshow' _ pair = "(" <> pshow (pfstBuiltin # pair) <> "," <> pshow (psndBuiltin # pair) <> ")"

-- | @since 1.10.0
instance PShow PPositive

-- | @since 1.10.0
instance PShow a => PShow (PMaybe a)
