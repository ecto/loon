# Loon Roadmap

> v0.1 shipped. Here's what's next.

## What's done (v0.1)

- [x] Logos lexer, recursive descent parser, AST
- [x] Tree-walking interpreter with 30+ builtins
- [x] Hindley-Milner type inference (Algorithm W)
- [x] Ownership & borrowing checker (lexical)
- [x] Algebraic effects with one-shot continuations
- [x] REPL with time travel and forking
- [x] Basic WASM codegen (integers, functions, strings, println)
- [x] CLI: `new`, `run`, `build`, `check`, `test`, `repl`, `explain`
- [x] 45 tests passing

---

## What's done (v0.2)

- [x] Destructuring in `let`: `[let [x y] point]`, `[let {name age} user]`
- [x] Destructuring in `fn` params: `[fn [[k v]] ...]`
- [x] Exhaustiveness checking for `match` (ADT constructors)
- [x] WASM indirect calls + environment passing for closures
- [x] Lambda lifting for simple cases
- [x] `[use module.path]`, `[use module {item1 item2}]` (interpreter)
- [x] File = module convention (interpreter)
- [x] `[pub defn ...]` / `[pub type ...]` visibility (interpreter)
- [x] Multi-file execution (interpreter, with cycle detection + caching)
- [x] `Result` / `Option` as proper ADTs (in prelude)
- [x] String: `join`, `trim`, `starts-with?`, `ends-with?`, `replace`
- [x] Vec: `zip`, `flatten`, `chunk`, `reverse`, `drop`, `find`
- [x] Map: `keys`, `values`, `merge`, `remove`
- [x] Pipe operator type checking
- [x] `word-count.loon` working
- [x] Clippy warnings clean

---

## v0.2.1 — Close the gaps

What's left to finish v0.2 properly.

### Module system in type checker & codegen
- [x] `[use ...]` resolved during type checking (cross-file inference)
- [x] `[pub ...]` visibility enforced in type checker
- [ ] Multi-file compilation to WASM (module linking/bundling)

### Closure capture classification
- [x] Ref vs move capture analysis (integrate with ownership checker)

---

## v0.3 — Types get serious

### Traits / protocols
- [ ] `[trait Display [fn display [self] → String]]`
- [ ] `[impl Display Shape ...]`
- [ ] Trait-based operator overloading (`Add`, `Eq`, `Ord`)
- [ ] Trait bounds in type inference

### Advanced type features
- [ ] Typed AST (separate from untyped — type checker produces typed tree)
- [ ] `[sig]` assertions checked against inferred types
- [ ] Row polymorphism for maps/records
- [ ] Type error messages with source spans (integrate with codespan-reporting)

### Ownership improvements
- [ ] Borrow inference per-parameter (read-only → immutable borrow, mutates → mutable borrow, escapes → move)
- [ ] Copy types: primitives auto-copy, `[#[derive Copy] type ...]`
- [ ] Better error messages: what/why/how format with visual ownership diagrams

---

## v0.4 — Effects for real

### Full effect system
- [ ] Effect inference: calling `IO.read-file` propagates `IO` to caller
- [ ] Effect annotations checked: `/ {IO Fail}` verified against inferred set
- [ ] `?` desugaring: `[expr]?` → match on Result, perform Fail on Err
- [ ] Partial handling: handle some effects, pass others through

### Built-in effects
- [ ] `IO`: file read/write, stdin/stdout, env vars (via WASI)
- [ ] `Fail`: Result integration, `?` sugar
- [ ] `Async`: placeholder runtime handler, mock handler for testing
- [ ] Channels: `[let [tx rx] [channel]]`, `[send tx val]`, `[recv rx]`

---

## v0.5 — WASM gets real

### Expanded codegen
- [x] Closures (indirect calls + captured environments)
- [ ] ADTs (tagged unions on the heap)
- [ ] Pattern matching compilation (decision trees)
- [ ] Persistent data structures (HAMT vectors and maps)
- [ ] String operations beyond literals
- [ ] WASI integration: file I/O, args, env

### Runtime
- [ ] `loon run` executes WASM via wasmtime (instead of interpreting)
- [ ] `loon build --release` with tree-shaking
- [ ] Target: hello world < 1KB, fib < 500 bytes

---

## v0.6 — Developer experience

### Error messages that teach
- [ ] Three-part errors: what / why / how to fix
- [ ] Visual ownership diagrams in error output
- [ ] `[explain EXXXX]` interactive REPL tutorials (not just text)
- [ ] Structured errors as data: `[catch-errors [compile "file.loon"]]`

### LSP server
- [ ] Go-to-definition
- [ ] Type-on-hover (the invisible type system, made visible)
- [ ] Autocomplete
- [ ] Inline diagnostics
- [ ] Inlay hints for inferred types (the three rendering modes from DESIGN.md)

### Formatter
- [ ] `loon fmt` — deterministic structural formatting
- [ ] No config, no debates — one true style

### Tree-sitter grammar
- [ ] Syntax highlighting for editors

---

## v0.7 — Macros

### Hygienic macros
- [ ] `[defmacro when [condition & body] ...]`
- [ ] Quasiquoting: `` ` ``, `~`, `~@`
- [ ] Hygiene by default (Scheme-style)

### Type-aware macros
- [ ] `[defmacro+ ...]` runs after type inference
- [ ] Access to type environment: `[type-fields T]`
- [ ] `[#[derive Debug Serialize Eq]]` implemented in Loon

---

## v0.8 — Pond (package manager)

### Core
- [ ] `loon.toml` dependency resolution
- [ ] Content-addressed packages (hash = identity)
- [ ] `pond add`, `pond verify`, `pond audit`
- [ ] `loon.lock` for reproducible builds

### Distribution
- [ ] Local file path dependencies
- [ ] Git dependencies (pin to commit hash)
- [ ] HTTP hosting
- [ ] IPFS (fully decentralized)

### Security
- [ ] Capability declarations in `loon.toml`
- [ ] `[use dep :grant [:net :fs.read [...]]]`
- [ ] `pond audit --capabilities`

---

## v1.0 — The legendary stuff

### Content-addressed definitions
- [ ] Every function/type identified by hash of its AST
- [ ] `[hash fn-name]`, `[history fn-name]`
- [ ] Rename refactoring doesn't change identity
- [ ] Dead code detection via hash reachability

### Incremental computation
- [ ] `[memo defn ...]` with automatic dependency tracking
- [ ] Salsa-inspired query engine
- [ ] The compiler uses this internally

### First-class LLM integration
- [ ] `[ai defn ...]` — compile-time code generation from prompts
- [ ] `[semantic defn ...]` — functions implemented by LLM calls
- [ ] `[agent ...]` — agent loops as a language primitive
- [ ] `[#[tool] defn ...]` — auto-generate tool schemas from types
- [ ] `ai.extract` — structured output via type → JSON schema
- [ ] Model config in `loon.toml`

### Persistent data structures
- [ ] HAMT-based persistent vectors and maps
- [ ] Structural sharing, reference counting
- [ ] `[Store.open]` — transparent persistence to disk

### Provenance tracking
- [ ] `[#[track] ...]` compile-time instrumentation
- [ ] `[provenance val]` — full origin chain
- [ ] Zero cost for untracked paths

### Notebooks
- [ ] `.loon.nb` — interleaved markdown and code cells
- [ ] Same type/ownership checking as regular files
- [ ] Compiles to WASM

---

## Non-goals for now

- Native (non-WASM) compilation target
- Backward compat with any existing LISP
- GUI framework (provide FFI, frameworks are packages)
- Multi-shot continuations (one-shot only, like Koka)

---

*Loon: a LISP that flies.*
