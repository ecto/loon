# Loon language card (compiler {{VERSION}})

Loon is a bracket lisp with inferred static types, ownership, and algebraic
effects. All calls are `[fn arg1 arg2]`. Comments start with `;`. Source
files use `.oo`.

## Semantics in one sentence each

- Truthiness: a value is truthy unless it says no (`false`) or says nothing (`()`, `None`).
- The falsy set is exactly `false`, `()`, and `None` — `0`, `""`, and empty collections are truthy. (None-falsy is the ruled semantics; if your build treats `None` as truthy, it predates the ruling.)
- Division by zero is a runtime error, not `0`, `Inf`, or `None`.
- A handler clause that does not call `resume` aborts: the rest of the handled computation is discarded and the clause body becomes the value of the `handle`.
- Maps preserve insertion order.
- Unknown effect operations are hard errors, never silent `()`.

## Syntax skeleton

```
[let x 42]                          ; binding (no def keyword)
[fn add [a b] [+ a b]]              ; named function
[fn [x] [* x x]]                    ; anonymous function
[if cond then-expr else-expr]       ; branches are single exprs; wrap in [do ...]
[match shape
  [Circle r] [* 3.14 [* r r]]
  Point      0.0
  _          -1.0]                  ; patterns destructure ADTs
[type Shape [Circle f64] Point]     ; ADT definition
[pipe #[1 2 3]
  [map [fn [x] [* x x]]]
  [filter [fn [x] [> x 2]]]]        ; pipe threads the value as the LAST arg
{:name "loon" :age 3}               ; map literal, keys are keywords
#[1 2 3]                            ; vector literal
:keyword                            ; keyword
"hi \(name), \([+ 1 2])"            ; string interpolation with \(expr)
[expr]?                             ; postfix ? unwraps Ok / re-raises Err as Fail.fail
[test adds [] [assert-eq [+ 1 1] 2]]  ; test form (run with loon test)
```

Gotchas: `and`/`or` and `if` all short-circuit. `fn` bodies allow multiple
exprs; `if` branches do not — wrap in `[do ...]`. There is no `nil`/`null`
and no `def`. `str` converts to string (no `to-string`). There is no
`if-let`.

## Option / Result

Prelude: `[type Option T [Some T] None]`, `[type Result T E [Ok T] [Err E]]`.

```
[match maybe-user
  [Some u] [greet u]
  None     "nobody"]
[get m :key "default"]              ; map lookup with default, no nil
[fn safe-div [a b]
  [if [= b 0] [Err "div by zero"] [Ok [/ a b]]]]
[fn use-it [] #{Fail}
  [let x [safe-div 10 2]?]          ; ? propagates the Err via the Fail effect
  [* x 100]]
[handle [use-it] [Fail.fail e] -1]  ; => 500; -1 on the Err path
```

## Effects in one paragraph

All nondeterminism (IO, network, randomness, failure) flows through effects.
Declare with `[effect Log [info [Str] Unit]]`; perform with `[Log.info "hi"]`;
a function's effect row is written after its params: `[fn f [] #{IO Fail} ...]`.
`handle` interprets performs: `[handle [work] [Log.info msg] [do [println msg]
[resume ()]]]` — `resume` continues the computation with the given value;
omitting `resume` aborts it. Handlers double as free mocks in tests (handle
`IO.read-file` with a canned string). Packages must be granted effects in
`pkg.oo` to use them (E0404 if not).

## Diagnostics

Errors carry what/why/fix and a code; `loon explain <code>` expands any code
into a tutorial. Top codes: E0100 unexpected character · E0101 unexpected
token · E0102 unclosed delimiter · E0200 type mismatch · E0201 unbound symbol
· E0202 arity mismatch · E0206 non-exhaustive match · E0300 use after move ·
E0400 unhandled effect · E0402 unknown effect operation · E0404 effect not
granted in pkg.oo · E0500 unresolved module · W0100 wildcard hides known
constructors.

## CLI

`loon check file.oo` (types + ownership; `--json` for JSONL diagnostics) ·
`loon run file.oo` (`--record trace.oo` to record effects) · `loon replay
trace.oo file.oo` (deterministic re-run, reproduces crashes) · `loon test
file.oo` · `loon fmt src/` · `loon explain E0403`.
