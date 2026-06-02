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

Output goes through WASI `fd_write`. Memory starts at 256 pages (16 MiB) because
the bump allocator never frees and vectors are copy-on-write.

## What compiles

- Arithmetic & comparison: `+ - * / %`, `> < = != <= >=`, `not and or`.
- `if`, `do`, `let`, multi-statement function bodies.
- Recursion, **self-tail-recursion** (`recur` in a `fn` loops back — no stack
  growth), and `loop`/`recur`.
- ADTs: `[type …]`, constructors, and `match` (int / nullary-ctor / field-binding
  arms, compiled to an if/else chain or a br_table).
- Closures, including captures, and passing functions as values.
- Strings: literals, variadic `str`/`str-concat`, `str-len`, `str-eq`, and
  `println` of a computed string (a fixpoint analysis tracks which functions
  return strings, since the value model has no runtime tag).
- Vectors: `#[…]` literals, `vec-new`/`vec-push`/`conj`/`vec-get`/`len`, and the
  higher-order functions `range`, `map`, `filter`, `each`, `fold` (the function
  argument may be a lambda literal or a named function).
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
- **Floats** — arithmetic assumes i64 operands, so float programs currently
  produce a module the validator rejects. Proper support needs a tagged or
  type-directed value model.
- **Maps / sets** as data (`{…}`, `#{…}` literals and their stdlib), and the
  string-processing stdlib (`split`, `lowercase`, …).

These run on the EIR VM, which implements the full language including one-shot
delimited continuations (see `samples/state.oo`).
