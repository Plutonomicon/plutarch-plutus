# Plutarch functions are strict

All Plutarch functions are strict. When you apply a Plutarch function to an argument using `papp` (or `#`/`#$` - synonyms to `papp`) - the argument will be evaluated before being passed into to the function. If you don't want the argument to be evaluated, you can use `pdelay`.

See: [Delay and Force](./../Introduction/Delay%20and%20Force.md).
