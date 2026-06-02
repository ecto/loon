# WASM codegen backend

`loon build <file.oo>` compiles a Loon program to a standalone
`target/<stem>.wasm` module that runs on any WASI runtime (node `--experimental-wasi`,
wasmtime, browsers with a WASI shim) with **no Loon host** — the compiler,
type checker, and interpreter are not present at runtime.

This is a second backend alongside the EIR VM (`loon run`, the default) and the
optional Cranelift JIT (`--features native`). It lives in
`crates/loon-lang/src/codegen/`.

## Value model

Every value is a raw **i64** (untagged):

- Integers and booleans are themselves.
- Strings are packed as `(ptr << 32) | len` into linear memory.
- ADTs / vectors are pointers to heap structures (bump-allocated; never freed).
- Closures are `(table_index << 32) | env_ptr`; the function is reached through
  the funcref table, the captures through `env_ptr`.

Floats are the exception to "everything is an i64": a float value is its f64
**bit pattern** stored in the i64 slot, reinterpreted to `f64` at each
arithmetic/comparison op. Whether an op is int or float is **type-directed** —
codegen consumes the checker's resolved `NodeId → Type` map (see "Type-directed"
below).

Output goes through WASI `fd_write`. Memory starts at 256 pages (16 MiB) because
the bump allocator never frees and vectors are copy-on-write.

## Type-directed

The backend runs the type checker over the macro-expanded program and keeps the
resolved type of every node. This drives decisions the untyped syntax can't:
`println`/arithmetic dispatch on `Int` vs `Float` vs `Str` from real types, not
guesses. Where the checker *generalizes* a polymorphic function body (so a body
node carries a type var rather than a concrete type), a small whole-program
structural fixpoint recovers which functions return floats / strings and which
params are float, propagated through call sites. Synthesized nodes (desugared
`pipe`/`when`/…) fall back to that structural path.

## What compiles

- Arithmetic & comparison: `+ - * / %`, `> < = != <= >=`, `not and or` — on
  both ints and **floats** (literals, arithmetic, comparison, and `println`,
  including through ADTs/`match` and function bodies).
- `inc dec abs min max mod`, `print`/`println`.
- `if`, `do`, `let`, multi-statement function bodies.
- Recursion, **self-tail-recursion** (`recur` in a `fn` loops back — no stack
  growth), and `loop`/`recur`.
- ADTs: `[type …]`, constructors, and `match` (int / nullary-ctor / field-binding
  arms, compiled to an if/else chain or a br_table).
- Closures, including captures, and passing functions as values.
- Strings: literals, variadic `str`/`str-concat` (which **stringifies non-string
  args** — type-directed Display via `int_to_str`), `str-len`, `str-eq`,
  `substring`, `char-at`, `lowercase`, `split`, and `println` of a computed
  string. Interpolation (`\(expr)` / `fmt`) desugars to `str` at parse time.
- Vectors: `#[…]` literals, `vec-new`/`vec-push`/`conj`/`cons`/`vec-get`/`len`/
  `first`/`empty?`/`take`, and the higher-order functions `range`, `map`,
  `filter`, `each`, `fold`, `sort-by` (the function argument may be a lambda, a
  named function, or a builtin — the latter two are wrapped automatically).
- Maps: `{:k v …}` literals and `assoc`/`get`/`update`/`contains?`/`keys`/
  `entries`/`group-by`. Keys compare **self-describingly** at runtime — raw
  equality, then string-content compare when both keys look like string pointers
  — so string-keyed maps work even through generic HOFs (e.g. a `fold`
  accumulator). `(a b)` tuple literals and tuple-destructuring params
  (`[fn [[k v]] …]`, `_` wildcard) for both closures and top-level fns.
- Keywords (`:foo`), `when`/`unless`/`cond`.
- `pipe` (thread-last).
- Multi-arity functions (`[fn f ([x] …) ([x y] …)]`) — each clause is its own
  function, resolved by argument count.
- Effect **operations**: `E.op args` lowers to a host-provided WASM import
  (`loon:effects/<effect>`); effect-row annotations on functions are ignored.
- Dead-code elimination (`tree_shake`) and a relocating function-index pass so
  imports and `_start` stay correct.

## What does *not* compile yet (use `loon run`)

- **Delimited continuations** — `handle` / `resume` / `try`. Capturing and
  resuming a stack segment needs a whole-program CPS / trampoline transform the
  backend doesn't do. Effect *operations* still compile (to imports); only the
  handlers don't.
- **Nested *named* local functions** (`[fn build [..] …]` inside a body) and
  **sets** (`#{…}`) as data.
- **Heterogeneous / fully-dynamic values.** The string-vs-pointer
  self-description used for map keys, `empty?`, and `str` covers homogeneous
  data; a genuinely mixed collection (or printing an arbitrary value of unknown
  type) would still need real value **tags**. Almost everything whose type is
  statically evident, or whose runtime shape is self-describing, works today.

These run on the EIR VM, which implements the full language including one-shot
delimited continuations (see `samples/state.oo`).
