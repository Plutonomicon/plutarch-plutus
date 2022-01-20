{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Plutarch.Generic (
  Color(..),
) where
    
import Plutarch.Integer
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Builtin
import Plutarch.Bool
import Plutarch.DataRepr
import Data.Foldable ( maximumBy )
import Data.List ( sortOn, groupBy )
import Data.SOP.Constraint (AllZipF)
import Plutarch.Prelude
import Plutarch

data Color (s :: S) = Red | Blue | Custom (Term s PInteger)
  deriving stock (GHC.Generic )
  -- deriving PIsData via (PIsDataReprInstances' Color)

instance Generic (Color s)

{- FIXME
    • Could not deduce: s1 ~ s
      from the context: (PIsDataRepr' s a,
                         DataReprTypes a s '[] (NS (NP (Term s)) (ToPType2 (Code (a s))))
                         ~ ToPType2 (Code (a s)))
        bound by the instance declaration

newtype PIsDataReprInstances' (a :: PType) (s :: S) = PIsDataReprInstances' (a s)

instance PIsDataRepr' s a => PIsData (PIsDataReprInstances' a) where
  pdata = punsafeCoerce
  pfromData = punsafeCoerce

instance (PIsDataRepr' s a,
  DataReprTypes a s '[] (NS (NP (Term s)) (ToPType2 (Code (a s)))) ~ ToPType2 (Code (a s))
 ) => PMatch (PIsDataReprInstances' a) where
  pmatch x f = pmatchRepr' @s @a (punsafeCoerce x) (f . PIsDataReprInstances')

-- Proof that there is a PIsData instance.
_colorData :: Term s (PAsData Color)
_colorData = pdata $ pcon Red
-}

{-
instance PIsData Color where
  pfromData x =
    plet (pasConstr # pforgetData x) $ \constr ->  
      plet (pfstBuiltin # constr) $ \idx ->
        pif' # (idx #== 0) 
          # pcon Red 
          # (pif' # (idx #== 1) 
            # pcon Blue 
            # (pif' # (idx #== 2) 
              # (plet (psndBuiltin # constr) $ \arg -> plet (pfromData . punsafeCoerce $ phead @PBuiltinList @PData # arg) $ \i -> pcon (Custom i) )
              # perror
              ))
  pdata c =
    pmatch c $ \case
      Red ->
        punsafeBuiltin PLC.ConstrData # pconstant @PInteger 0 # nil
      Blue ->
        punsafeBuiltin PLC.ConstrData # pconstant @PInteger 1 # nil
      Custom i ->
        punsafeBuiltin PLC.ConstrData # pconstant @PInteger 2 # psingleton @PBuiltinList @PData # pdata i
    where
      nil :: Term s (PBuiltinList PData)
      nil = pnil
-}


pRep :: AllZipN (Prod SOP) (LiftedCoercible I (Term s)) xss (ToPType2 xss) => SOP I xss -> SOP (Term s) (ToPType2 xss)
pRep = hcoerce

hRep ::
  ( AllZipF (AllZip (LiftedCoercible (Term s) I)) (ToPType2 xss) xss,
    SameShapeAs (ToPType2 xss) xss,
    SameShapeAs xss (ToPType2 xss),
    All Top (ToPType2 xss),
    All Top xss
  ) =>
  SOP (Term s) (ToPType2 xss) ->
  SOP I xss
hRep = hcoerce

_colorRep :: SOP I '[ '[], '[], '[Term s PInteger]]
_colorRep = from (Red :: Color s)

_pcolorRep :: SOP (Term s) '[ '[], '[], '[PInteger]]
_pcolorRep = pRep _colorRep

type UnTerm :: Type -> PType
type family UnTerm x where
  UnTerm (Term s a) = a

-- Unfortunately we can't write a generic FMap due to ghc's arity limitations.
type ToPType :: [Type] -> [PType]
type family ToPType as where
  ToPType '[] = '[]
  ToPType (a ': as) = UnTerm a ': ToPType as

type ToPType2 :: [[Type]] -> [[PType]]
type family ToPType2 as where
  ToPType2 '[] = '[]
  ToPType2 (a ': as) = ToPType a ': ToPType2 as

-- Get a Plutarch sum-of-products that uses `Term s` instead of identify functor `I`.
type family PSOP sop s where
  PSOP (SOP I xss) s = SOP (Term s) (ToPType2 xss)

-- `smus` is the previous sum constructors in reverse order (already traversed).
-- `sums` is the rest of the sum constructors. `mkDataReprHandlers` will
-- recursively construct DataReprHandlers.
class MkDataReprHandlers (a :: PType) (s :: S) (smus :: [[PType]]) sums where
  type DataReprTypes a s smus sums :: [[PType]]
  mkDataReprHandlers :: forall b. (a s -> Term s b) -> DataReprHandlers b (DataReprTypes a s smus sums) s

instance MkDataReprHandlers a s smus (NS (Term s) '[]) where
  type DataReprTypes _ _ _ _ = '[]
  mkDataReprHandlers _ = DRHNil

instance
  ( Generic (a s),
    xall ~ Code (a s),
    pall ~ Append smus (sum : sums),
    ToPType2 xall ~ pall,
    AllZipF (AllZip (LiftedCoercible (Term s) I)) pall xall,
    SameShapeAs pall xall,
    SameShapeAs xall pall,
    All Top pall,
    MkSum smus sum sums,
    FromPDataList s sum,
    MkDataReprHandlers (a :: PType) (s :: S) (sum : smus) (NS (NP (Term s)) sums)
  ) =>
  MkDataReprHandlers (a :: PType) (s :: S) smus (NS (NP (Term s)) (sum : sums)) where
  -- TODO: There ought to be a simpler/fast way to compute this.
  type DataReprTypes a s smus (NS (NP (Term s)) (sum : sums)) = 
      (sum : DataReprTypes a s (sum : smus) (NS (NP (Term s)) sums))
  mkDataReprHandlers f =
    DRHCons
      (f . to . hRep . SOP . mkSum' @smus @sum @sums .  gFromPDataList)
      $ mkDataReprHandlers @a @s @(sum : smus) @(NS (NP (Term s)) sums) f

-- | TODO: We should probably use the injections from `generics-sop`.
class MkSum (before :: [[PType]]) (x :: [PType]) (xs :: [[PType]]) where
  mkSum' :: NP (Term s) x -> NS (NP (Term s)) (Append before (x ': xs))

instance MkSum '[] x xs where
  mkSum' = Z

instance MkSum bs x xs => MkSum (b ': bs) x xs where
  mkSum' = S . mkSum' @bs @x @xs

type family Append (a :: [as]) (b :: [as]) :: [as] where
  Append '[] b = b
  Append (a ': as) b = a ': Append as b

class FromPDataList s as where
  gFromPDataList :: Term s (PDataList as) -> NP (Term s) as

instance FromPDataList s '[] where
  gFromPDataList _ = hcpure (Proxy @PIsData) undefined

instance (FromPDataList s xs, PIsData x) => FromPDataList s (x ': xs) where
  gFromPDataList dat = pfromData (pdhead # dat) :* gFromPDataList @s @xs (pdtail # dat)

type family MkPDataRepr (a :: PType) (s :: S) (as :: [[Type]]) where
  MkPDataRepr a s '[] = PDataRepr '[]
  MkPDataRepr a s (def : defs) = PDataRepr (ToPType def : ToPType2 defs)

type family UnSOP a where
  UnSOP (SOP f xss) = xss

instance
  ( Generic (f s),
    PIsData f,
    PlutusType s f,
    MkDataReprHandlers f s '[] (NS (NP (Term s)) (UnSOP (PSOP (Rep (f s)) s))),
    DataReprTypes f s '[] (NS (NP (Term s)) (ToPType2 (Code (f s)))) ~ ToPType2 (Code (f s))
  ) =>
  PIsDataRepr s f
  where
  type PIsDataReprRepr s f = UnSOP (PSOP (Rep (f s)) s)
  pmatchRepr dat mk =
    pmatchDataRepr' dat $
      mkDataReprHandlers @f @s @'[] @(NS (NP (Term s)) (UnSOP (PSOP (Rep (f s)) s))) mk

pmatchDataRepr' :: Term s (PDataRepr defs) -> DataReprHandlers out defs s -> Term s out
pmatchDataRepr' d handlers =
  plet (pasConstr #$ punsafeCoerce d) $ \d' ->
    plet (pfstBuiltin # d') $ \constr ->
      plet (psndBuiltin # d') $ \args ->
        let handlers' = applyHandlers args handlers
         in runTermCont (findCommon handlers') $ \common ->
              go
                common
                0
                handlers'
                constr
  where
    hashHandlers :: [Term s out] -> TermCont s [(Dig, Term s out)]
    hashHandlers [] = pure []
    hashHandlers (handler : rest) = do
      hash <- hashOpenTerm handler
      hashes <- hashHandlers rest
      pure $ (hash, handler) : hashes

    findCommon :: [Term s out] -> TermCont s (Dig, Term s out)
    findCommon handlers' = do
      l <- hashHandlers handlers'
      pure $ head . maximumBy (\x y -> length x `compare` length y) . groupBy (\x y -> fst x == fst y) . sortOn fst $ l

    applyHandlers :: Term s (PBuiltinList PData) -> DataReprHandlers out defs s -> [Term s out]
    applyHandlers _ DRHNil = []
    applyHandlers args (DRHCons handler rest) = handler (punsafeCoerce args) : applyHandlers args rest

    go ::
      (Dig, Term s out) ->
      Integer ->
      [Term s out] ->
      Term s PInteger ->
      Term s out
    go common _ [] _ = snd common
    go common idx (handler : rest) constr =
      runTermCont (hashOpenTerm handler) $ \hhash ->
        if hhash == fst common
          then go common (idx + 1) rest constr
          else
            pif
              (fromInteger idx #== constr)
              handler
              $ go common (idx + 1) rest constr

{-
instance PIsDataRepr Color where
  {-
  type PIsDataReprRepr Color
    = '[ '[]
       , '[]
       , '[PInteger]
       ]
  -}
  type PIsDataReprRepr Color = ToPType2 (UnSopT (GHC.Rep Color))

  pmatchRepr dat f =
    pmatchDataRepr dat $
      -- TODO: Should we pass the empty list and check here?
      DRHCons (const $ f Red) $
        DRHCons (const $ f Blue) $
          DRHCons
            (f . Custom . pfromData . (pdhead #))
            DRHNil
-}

instance PlutusType s Color where
  -- Scott-encode inner representation
  type PInner Color c = c :--> c :--> (PInteger :--> c) :--> c

  pcon' = \case
    Red -> plam $ \(c1 :: Term s _) _ _ -> c1
    Blue -> plam $ \_ (c2 :: Term s _) _ -> c2
    Custom c -> plam $ \_ _ c3 -> c3 # c

  pmatch' x f =
    x
      # f Red
      # f Blue
      # (plam $ \i -> f (Custom i))

instance PEq Color where
  c1 #== c2 =
    pmatch c1 $ \c1' -> pmatch c2 $ \c2' ->
      case (c1', c2') of
        (Red, Red) -> pcon PTrue
        (Blue, Blue) -> pcon PTrue
        (Custom i1, Custom i2) -> i1 #== i2
        _ -> pcon PFalse
