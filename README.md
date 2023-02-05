### The Tiny Schemer

Interpreter for language with type inference and support for recursion, mutation and continuations written in OCaml.

#### Notes

- Language supports let-polymorphism and [classical type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
with value restriction to guarantee soundness of the system.

- [Continuations](https://en.wikipedia.org/wiki/Continuation) are available for use,
but as they are more dynamic language construct (Scheme), I don't perform static type analysis in some examples
(e.g. [coroutines](https://github.com/Bohun9/The-Tiny-Schemer/blob/master/test/code/coroutines)).

- Recursion is added as new language construct, but it could be expressed as syntantic sugar by
[untying the recursive knot technique](https://github.com/Bohun9/The-Tiny-Schemer/blob/master/test/code/fact_knot).

- Interpreter is written in [continutation passing style](https://en.wikipedia.org/wiki/Continuation-passing_style).

#### Dependencies

- ounit2 (available via opam)

#### Running tests

`$ dune build && dune runtest`
