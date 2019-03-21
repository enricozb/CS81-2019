# typed_uscheme

## TODO
- Add tuples
- Add ADT
- Figure out mutation for values
- Add `not` operator
- Check out bidirectional types
- Add TCO
- Relax the value restriction according to
  [this paper](https://caml.inria.fr/pub/papers/garrigue-value_restriction-fiwflp04.pdf)

## Bugs
### Check-* should add type constraints
```
let mut x = []
check_expect x, [1,2,3]
check_expect x, [[]]
```
typechecks successfully. This should not happen. The constraints occuring here
should cause type restrictions on `x`.

### Whitespace in file_input
```
def f():
  def g():
    return 0

  return g()
```
causes a parser error because of the double newline...


## Questions
- Why does `Value.Closure` need `(fun () -> val_env)`
  instead of just `val_env`?

