# CS81 2019 - myth language attempt 1

## Resources
### Other Languages & Repos
- [Crux](https://github.com/cruxlang/)
  - A very similar idea but for JavaScript. Supports mutability and seems
    to nicely solve something like the value restriction.
- [Crystal](https://crystal-lang.org/)
  - A similar idea. A type-safe Ruby-like language that compiles to C.
- [Type-Systems](https://github.com/tomprimozic/type-systems)
  - Several type systems implemented im OCaml.
- [Airtight](https://github.com/alehander42/Airtight)
  - A potentially useful toy language. Python 3 with hindley-milner.

### Papers
- [How OCaml type checker works](http://okmij.org/ftp/ML/generalization.html#introduction)
  - useful for understanding generalization & use of levels for efficient
    generalization

### Ideology
We want it to write as closely to Python as possible. Something close to
"duck typing", no massive care for memory, and type safety. Type inference
is important but 100% inference is useless. Function signatures should be
explicitly typed. Swift has a very good amount of inference, and this
language should aspire to that amount of inference.

### More Details
See [wiki](https://github.com/enricozb/CS81-2019/wiki).

