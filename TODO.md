# typed_uscheme

## TODO
- Clean up the stupid stuff at the bottom of lexer.mll
- Add tuples
- Add empty return
- Add GADTs
- Relax the value restriction according to
  [this paper](https://caml.inria.fr/pub/papers/garrigue-value_restriction-fiwflp04.pdf)
- Figure out mutation for values
- Add `not` operator
- Add TCO
- Add debug mode for printing tokens
- Check out bidirectional types

## Bugs
### Check-* should add type constraints
```
let mut x = []
check_expect x, [1,2,3]
check_expect x, [[]]
```
typechecks successfully. This should not happen. The constraints occuring here
should cause type restrictions on `x`.

## Questions
- Why does `Value.Closure` need `(fun () -> val_env)`
  instead of just `val_env`?

