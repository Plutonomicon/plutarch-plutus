# `plutarch-numeric`

## What is this?

## What can this do?

## What are the goals of this project?

### Lawfulness

Type classes must have laws, and those laws should inform what instances are
valid and possible. Given the core nature of any numerical hierarchy, in
Plutarch or elsewhere, any laws governing such must be especially well-documented 
and thorough. We aim to put the laws first in all our type classes and
implementations: you know _exactly_ what you are getting. Furthemore, we explain
not only what the laws are, but what they mean relative the types we provide, as
well as Plutarch types generally.

### Generality

Many numerical operations are general across many kinds of 'number';
mathematically, these notions are used fairly freely. Due to the type system
being more demanding than such 'fast and loose' reasoning allows, we need to
have an exact definition of the 'limits' of any such capability. We aim to work
as close to these limits as possible - if it makes sense for a numerical
operation to be supported, we should have that be possible.

### Correctness by construction

Making invalid states unrepresentable is an important Haskell practice; with it,
you can avoid a whole range of problems before they even arise. We aim to use
correct-by-construction approaches in everything we do, thus avoiding problems
of invalid state at a fundamental level.

## What can I do with this?

The code is licensed under MIT; check the LICENSE file for details.
