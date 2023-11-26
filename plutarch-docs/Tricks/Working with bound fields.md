<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.WorkingWithBoundFields (foo, foo', coreValidator) where 

import Plutarch.Prelude
import Plutarch.DataRepr (HRec, HRecOf, PDataFields, PMemberFields)
import Plutarch.Api.V1 (PTxInfo, PScriptContext)
import Plutarch.Extra.TermCont (pmatchC)
```

</p>
</details>

# Working with bound fields yielded by `pletFields`

You may have noticed that `pletFields` actually returns a Haskell level heterogenous list, with all the interesting fields 
"bound" to it. Only the fields you actually use from these bindings are extracted and put into the resulting script. Therefore,
you _only pay for what you use_.

```hs
pletFields ::
  forall fs a s b ps bs.
  ( PDataFields a
  , ps ~ (PFields a)
  , bs ~ (Bindings ps fs)
  , BindFields ps bs
  ) =>
  Term s a ->
  (HRecOf a fs s -> Term s b) ->
  Term s b
```

The real juice of that massive type is the `HRecOf`, which is a utility type alias you can use in functions that operate on the return value of `pletFields`:

```haskell
newtype PFooType s 
  = PFooType (Term s (PDataRecord '["frst" ':= PInteger, "scnd" ':= PBool, "thrd" ':= PString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields)
instance DerivePlutusType PFooType where type DPTStrat _ = PlutusTypeData

foo :: HRecOf (PAsData PFooType) '[ "frst", "scnd" ] s -> Term s PInteger
foo h = pif (getField @"scnd" h) (getField @"frst" h) 0
```

> Note: Be careful to derive `PDataFields` on your type if it has only one constructor

This is very useful for single use functions that you use as "branches" in your validators - they work more like macros or templates rather than real functions. For example, you might have different branches for different constructors of a redeemer, but all branches end up needing to do common field extraction. You could abstract it out using:

```haskell
data PSomeRedm s 
  = FirstRedm (Term s (PDataRecord '[]))
  | SecondRedm (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType PSomeRedm where type DPTStrat _ = PlutusTypeData

firstRedmCheck :: 
  HRecOf PTxInfo '[ "inputs", "outputs", "mint", "datums" ] s 
  -> TermCont s (Term s PUnit)
firstRedmCheck _info = do
  -- Do checks with info fields here.
  pure $ pconstant ()

secondRedmCheck ::
  HRecOf PTxInfo '[ "inputs", "outputs", "mint", "datums" ] s 
  -> TermCont s (Term s PUnit)
secondRedmCheck _info = do
  -- Do checks with info fields here.
  pure $ pconstant ()

coreValidator :: Term s (PData :--> PAsData PSomeRedm :--> PScriptContext :--> PUnit)
coreValidator = plam $ \_ (pfromData -> redm) ctx' -> unTermCont $ do
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  info <- tcont $ pletFields @'["inputs", "outputs", "mint", "datums"] $ getField @"txInfo" ctx
  pmatchC redm >>= \case
    FirstRedm _ -> firstRedmCheck info
    SecondRedm _ -> secondRedmCheck info
```

Without it, you may have to fallback to deconstructing `info` with `pletFields` in every single branch.

However, this is rather _nominal_. What if you don't need the exact same fields in all 
branches? Let's go back to the example with `foo` and `FooType`. What if someone has:

```hs
fooTypeHrec <- tcont $ pletFields @'["frst", "scnd", "thrd"] fooTypeValue
foo fooTypeHrec
-- uh oh
```

The type required by `foo` should _morally_ work just fine with `fooTypeHrec`, but it won't! 
What we really want, is some sort of row polymorphism. This is where the `PMemberFields` type 
from `Plutarch.DataRepr` comes in:

```haskell
foo' :: PMemberFields PFooType '["scnd", "frst"] s as => HRec as -> Term s PInteger
foo' h = pif (getField @"scnd" h) (getField @"frst" h) 0
```

Now `foo` merely requires the `HRec` to have the `"scnd"` and `"frst"` fields from `PFooType`, more fields are allowed just fine!
