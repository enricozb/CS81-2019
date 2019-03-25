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

### Papers & Readings
- [Modifiable Data Structures In OCaml](https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora026.html)
  - Used to implement value restriction. Especially the very bottom section.
- [Relaxing The Value Restriction](https://caml.inria.fr/pub/papers/garrigue-value_restriction-fiwflp04.pdf)
- [MLsub - Hindley Milner + Subtyping](https://www.cl.cam.ac.uk/~sd601/mlsub/)
- [Typeclasses & Hindley Milner](https://boxbase.org/entries/2018/apr/16/typeclasses/)
- [How OCaml type checker works](http://okmij.org/ftp/ML/generalization.html#introduction)
  - Used to implement generalization with levels for efficiency.

### Ideology
We want it to write as closely to Python as possible. Something close to
"duck typing", no massive care for memory, and type safety. Type inference
is important but 100% inference is useless. Function signatures should be
explicitly typed. Swift has a very good amount of inference, and this
language should aspire to that amount of inference.

### More Details
See [wiki](https://github.com/enricozb/CS81-2019/wiki).

