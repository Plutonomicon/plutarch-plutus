module Plutarch.Rational (
  PRational (..),
  preduce,
  pnumerator,
  pdenominator,
  pfromInteger,
  pround,
  ptruncate,
  pproperFraction,
) where

import Data.Ratio (denominator, numerator)
import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (
  PlutusType (..),
  Term,
  pcon,
  pfix,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (PEq (..), POrd (..), pif)
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData,
  pasInt,
  pasList,
  pdata,
  pdataImpl,
  pforgetData,
  pfromDataImpl,
 )
import Plutarch.Integer (PInteger, PIntegral (pdiv, pmod))
import Plutarch.List (PListLike (pcons, phead, pnil, ptail), pmap)
import Plutarch.Pair (PPair (..))
import Plutarch.Show (PShow (pshow'), pshow)
import Plutarch.Trace (ptraceError)
import Plutarch.Unsafe (punsafeCoerce)

data PRational s
  = PRational
      ( Term
          s
          ( PDataRecord
              '[ "numerator" ':= PInteger
               , "denominator" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields, PEq)
    via PIsDataReprInstances PScriptContext

instance PUnsafeLiftDecl PScriptContext where type PLifted PScriptContext = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance PConstantDecl Plutus.ScriptContext
