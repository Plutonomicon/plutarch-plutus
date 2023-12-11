<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.WorkDuplication (abs, abs', pf, pf') where 
import Plutarch.Prelude
import Prelude hiding (abs)
```

</p>
</details>

# Don't duplicate work

Haskell bindings are simply "inlined" during Plutarch compilation.

Consider the simple snippet:

```haskell
pf :: Term s PInteger
pf =
  let 
    foo :: forall s. Term s PInteger
    foo = 1 + 2      -- | A Haskell binding.
  in pif
       (foo #== 3)     -- | A.) ...then inline here...
       foo             -- | B.) ...and inline here.
       7
```

Using the `printTerm` function (provided by the top level `Plutarch` module), we can view
the computation bound to `foo`. The formatting below is our own; notice that
`foo`, which becomes `(addInteger 1 2)` in UPLC, is inlined twice:

```hs
> printTerm pf

(...)

(force
    (force ifThenElse
       (equalsInteger
          (addInteger 1 2)      -- | A.) `foo` appears here...
          3
       )
       (delay (addInteger 1 2)) -- | B.) ...and here
       (delay 7)
    )
)
```

Performing this computation twice is obviously bad (in this circumstance), since it will increase
the execution budget for the script.

A technique to circumvent this is to introduce a free variable via a lambda,
replace the inlined expression (in our case, `(addInteger 1 2)`) with that variable, and them
apply the lambda to the _calculated_ expression:

```hs
> printTerm pf'

(...)

((\\i0 ->                          -- | A'.) Introduce a lambda here,...
    force
        (force ifThenElse
            (equalsInteger i1 3)   -- | B'.) ...apply the argument here,...
            (delay i1)             -- | C'.) ...and apply the argument here,
            (delay 7)
        )
 ) (addInteger 1 2)                -- | D'.) ...then calculate `foo` once and apply the lambda
)
```

Plutarch provides the `plet :: Term s a -> (Term s a -> Term s b) -> Term s b` function
to accomplish exactly this. To demonstrate this technique, the implementation of `pf'` that
will lead to the above UPLC is given as:

```haskell
{-
Note: the letter labels on our annotations match the operations in the
previous example.
-}

pf' :: Term s PInteger
pf' =
  plet (1 + 2) $             -- | D.') Calculate the desired value here (strictly),...
  \foo ->                    -- | A.') ...introduce a lambda abstraction,...
    pif
      (foo #== 3)            -- | B.') ...and apply the argument here...
      foo                    -- | C.') ... and here.
      7
```

Another example of this would be:

```haskell
abs :: Term s PInteger -> Term s PInteger
abs x = pif (x #<= -1) (negate x) x
```

Guess what would happen if you used it like:

```hs
abs (reallyExpensiveFunction # arg)
```

It'd turn into:

```hs
pif ((reallyExpensiveFunction # arg) #<= -1) (negate (reallyExpensiveFunction # arg)) (reallyExpensiveFunction # arg)
```

Oh no. `reallyExpensiveFunction` is going to be _applied three times_. That's 3 times the cost!

Instead, consider using `plet`:

```haskell
abs' :: Term s PInteger -> Term s PInteger
abs' x' = plet x' $ \x -> pif (x #<= -1) (negate x) x
```

Of course, what you _really_ should do , is prefer Plutarch level functions whenever possible. Since arguments to Plutarch level functions are pre-evaluated and those bindings are completely ok to use as many times as you want!

## Where should arguments be `plet`ed?

You don't have to worry about work duplication on arguments in _every single scenario_. In particular, the argument to `plam` is also a Haskell function, isn't it? But you don't need to worry about `plet`ing your arguments there since it becomes a Plutarch level function through `plam` - thus, all the arguments are evaluated before being passed in.

Where else is `plet` unnecessary? Functions taking in continuations, such as `plet` (duh) and `pletFields`, always pre-evaluate the binding. An exception, however, is `pmatch`. In certain cases, you don't need to `plet` bindings within the `pmatch` case handler. For example, if you use `pmatch` on a `PList`, the `x` and `xs` in the `PSCons x xs` _will always be pre-evaluated_. On the other hand, if you use `pmatch` on a `PBuiltinList`, the `x` and `xs` in the `PCons x xs` _are **not** pre-evaluated_. Be sure to `plet` them if you use them several times!

In general, `plet`ing something back-to-back several times will be optimized to a singular `plet` anyway. However, you should know that for data encoded types (types that follow "[implementing `PIsDataRepr` and friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)") and Scott encoded types, `pmatch` handlers get pre-evaluated bindings. For `PBuiltinList`, and `PDataRecord` - the bindings are not pre-evaluated.

You should also `plet` local bindings! In particular, if you applied a function (Plutarch level or Haskell level) to obtain a value, then bound that value to a variable e.g. with `let` or `where`, then avoid using it multiple times. The binding will simply get inlined as the function application - and it'll keep getting re-evaluated. You should `plet` it first!

This also applies to field accesses using `OverloadedRecordDot`. When you do `ctx.purpose`, it really gets translated to `getField @"purpose" ctx`, which is a function call! If you use the field multiple times, `plet` it first.

Another slightly obscure case can be observed in scott encoded types. When you build a scott encoded type using `pcon` - the Plutarch terms you use as fields are simply inlined within the scott encoded type. As such, `pcon $ PPair <complex expr> <another complex expr>` ends up like:

```hs
(\f -> f <complex expr> <another complex expr>)
```

This is practically pseudocode. However, it demonstrates that your expressions are not evaluated when _building_ the scott encoded pair. Indeed, they will be evaluated when you `pmatch` on it. As such, if you `pmatch` on this pair multiple times, those expressions will evaluate multiple times!

If you _must_ `pmatch` on such types several times, `plet` the fields before building the container type!
