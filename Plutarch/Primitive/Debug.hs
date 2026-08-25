{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Primitive.Debug (
  PDebug (..),
  pshow,
) where

import Control.Monad.Reader (ask)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector.NonEmpty qualified as NEVector
import Numeric.Natural (Natural)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term (Term),
  TermEnv (TermEnv),
  TracingMode (NoTracing),
  asRawTerm,
  pfix,
  plam',
  punsafeCase,
  punsafeConstant,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
  pgeneralize,
  punsafeSpecialize,
  (#),
  (#$),
 )
import Plutarch.Primitive.Bool (PBool, pif)
import Plutarch.Primitive.BuiltinFun (
  paddInteger,
  pappendString,
  pchooseData,
  pequalsInteger,
  pindexByteString,
  plengthOfByteString,
  pquotientInteger,
  premainderInteger,
  psubtractInteger,
  punBData,
  punConstrData,
  punIData,
  punListData,
  punMapData,
 )
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Eq ((#==))
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList (PBCons, PBNil))
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Numeric (PByte, PInteger, PNatural, PPositive)
import Plutarch.Primitive.Ord ((#<=))
import Plutarch.Primitive.Pair (PBPair (PBPair))
import Plutarch.Primitive.String (PString)
import PlutusCore qualified as PLC

-- | @since wip
class PlutarchType a => PDebug (a :: S -> Type) where
  pdebug ::
    forall (r :: S -> Type) (s :: S).
    PlutarchType r =>
    Term s a -> (Term s PString -> Term s r) -> Term s r
  default pdebug ::
    forall (r :: S -> Type) (s :: S).
    (PlutarchType r, PDebug (PRepresentation a)) =>
    Term s a -> (Term s PString -> Term s r) -> Term s r
  pdebug t = pdebug (pcoerce t)

-- | @since wip
instance PDebug PBool where
  pdebug t f = f (pif t (sc "true") (sc "false"))

-- | @since wip
instance PDebug PInteger where
  pdebug t f =
    f
      ( pif
          (t #<= ic (-1))
          -- Add a minus sign, absolute value
          (pappendString # sc "-" # pshow (punsafeSpecialize @PNatural (psubtractInteger # ic 0 # t)))
          (pshow (punsafeSpecialize @PNatural t))
      )

-- | @since wip
instance PDebug PNatural where
  pdebug t f =
    f
      ( pif
          (t #== nc 0)
          (sc "0")
          ( let q = pquotientInteger # pcoerce t # ic 10
                r = premainderInteger # pcoerce t # ic 10
             in renderNum # q # r
          )
      )

-- | @since wip
instance PDebug PPositive where
  pdebug t f =
    f
      ( let q = pquotientInteger # pgeneralize t # ic 10
            r = premainderInteger # pgeneralize t # ic 10
         in renderNum # q # r
      )

-- | @since wip
instance PDebug PByte where
  pdebug t f =
    f
      ( let t' = pgeneralize t
            d1 = pquotientInteger # t' # ic 16
            d2 = premainderInteger # t' # ic 16
         in pappendString # sc "0x" #$ pappendString # (renderHexDigit # d1) #$ renderHexDigit # d2
      )

-- | @since wip
instance PDebug PByteString where
  pdebug t f =
    f
      ( let len = pcoerce (plengthOfByteString # t)
         in pif
              (pequalsInteger # len # ic 0)
              (sc "[]")
              ( let lim = psubtractInteger # len # ic 1
                 in pappendString # sc "[" #$ pappendString # (go # t # lim # ic 0) # sc "]"
              )
      )
    where
      go :: forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger :--> PString)
      go = pfix $ \self -> plam' $ \bs -> plam' $ \lim -> plam' $ \i ->
        pif
          (pequalsInteger # lim # i)
          (pshow $ pindexByteString # bs # punsafeSpecialize i)
          ( let rendered = pshow $ pindexByteString # bs # punsafeSpecialize i
             in pappendString # rendered #$ pappendString # sc ", " #$ self # bs # lim #$ paddInteger # i # ic 1
          )

-- | @since wip
instance PDebug a => PDebug (PBList a) where
  pdebug t f =
    f
      ( pmatch t $ \case
          PBNil -> sc "[]"
          PBCons x xs -> pappendString # sc "[" #$ pappendString # (go # x # xs) # sc "]"
      )
    where
      go :: forall (s :: S). Term s (a :--> PBList a :--> PString)
      go = pfix $ \self -> plam' $ \x -> plam' $ \xs -> pmatch xs $ \case
        PBNil -> pshow x
        PBCons y ys -> pappendString # pshow x #$ pappendString # sc ", " #$ self # y # ys

-- | @since wip
instance (PDebug a, PDebug b) => PDebug (PBPair a b) where
  pdebug t f =
    f
      ( pmatch t $ \(PBPair x y) ->
          pappendString
            # sc "("
            #$ pappendString
            # pshow x
            #$ pappendString
            # sc ", "
            #$ pappendString
            # pshow y
            # sc ")"
      )

-- | @since wip
instance PDebug PData where
  pdebug ::
    forall (r :: S -> Type) (s :: S).
    PlutarchType r =>
    Term s PData -> (Term s PString -> Term s r) -> Term s r
  pdebug t f =
    pchooseData
      # t
      # pdebug asConstr f
      # pdebug asMap f
      # pdebug asList f
      # pdebug asI f
      # pdebug asB f
    where
      asConstr :: Term s (PBPair PInteger (PBList PData))
      asConstr = punConstrData # t
      asMap :: Term s (PBList (PBPair PData PData))
      asMap = punMapData # t
      asList :: Term s (PBList PData)
      asList = punListData # t
      asI :: Term s PInteger
      asI = punIData # t
      asB :: Term s PByteString
      asB = punBData # t

-- | @since wip
instance PDebug (PAsData a)

-- | @since wip
instance PDebug PString where
  pdebug t f = f (pappendString # sc "\"" #$ pappendString # t # sc "\"")

-- | @since wip
pshow ::
  forall (a :: S -> Type) (s :: S).
  PDebug a =>
  Term s a -> Term s PString
pshow t = pdebug t $ \asString -> Term $ do
  TermEnv tracing _ <- ask
  asRawTerm $ case tracing of
    NoTracing -> sc ""
    _ -> asString

-- Helpers

-- 'String constant'
sc :: forall (s :: S). Text -> Term s PString
sc = punsafeConstant . PLC.someValue

-- 'Integer constant'
ic :: forall (s :: S). Integer -> Term s PInteger
ic = punsafeConstant . PLC.someValue

-- 'Natural constant'
nc :: forall (s :: S). Natural -> Term s PNatural
nc = punsafeConstant . PLC.someValue @Integer . fromIntegral

renderNum :: forall (s :: S). Term s (PInteger :--> PInteger :--> PString)
renderNum = pfix $ \self -> plam' $ \q -> plam' $ \r ->
  pif
    (q #== ic 0)
    (renderDigit # r)
    ( let q' = pquotientInteger # q # ic 10
          r' = premainderInteger # q # ic 10
       in pappendString # (self # q' # r') #$ renderDigit # r
    )

renderDigit :: forall (s :: S). Term s (PInteger :--> PString)
renderDigit = plam' $ \i -> punsafeCase i . NEVector.generate1 10 $ toSomeTerm . sc . Text.pack . show

renderHexDigit :: forall (s :: S). Term s (PInteger :--> PString)
renderHexDigit = plam' $ \i ->
  punsafeCase i . NEVector.generate1 16 $
    toSomeTerm . sc . \case
      0 -> "0"
      1 -> "1"
      2 -> "2"
      3 -> "3"
      4 -> "4"
      5 -> "5"
      6 -> "6"
      7 -> "7"
      8 -> "8"
      9 -> "9"
      10 -> "A"
      11 -> "B"
      12 -> "C"
      13 -> "D"
      14 -> "E"
      _ -> "F"
