# Loon Roadmap

> v0.1 shipped. Here's what's next.

## What's done (v0.1)

- [x] Logos lexer, recursive descent parser, AST
- [x] Tree-walking interpreter with 30+ builtins
- [x] Hindley-Milner type inference (Algorithm W)
- [x] Ownership & borrowing checker (lexical)
- [x] Algebraic effects with multi-shot delimited continuations (EIR VM + WASM/CPS)
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
- [x] `word-count.oo` working
- [x] Clippy warnings clean

---

## What's done (v0.7) — Effects substrate & frameworks

- [x] Multi-shot delimited continuations on the default register VM
- [x] Handler-stack isolation across sequential/nested handles (correctness)
- [x] Escaping/answer-passing handlers type-check; generic ADT construction fixed
- [x] Host effects in the VM: clock, millis, uuid, env, file read/write
- [x] Real TCP/HTTP sockets in the VM (`Net.listen`/`accept`/`send`) — serve HTTP from Loon
- [x] Multi-file `use` runs on the register VM (qualified, selective, transitive)
- [x] Shared effect substrate (`src/eff`): IO/Clock/Random/Env/Reader/Log/Fail/State, prod/test/replay towers, composition
- [x] Async as effects: cooperative scheduler (`spawn`/`yield`/`cancel`), structured concurrency
- [x] HTTP framework (`src/http`): capability-typed routes, prod/test/trace towers, consume-on-send bodies
- [x] Agent framework (`src/agent`): Llm/Tool/Approval/Memory, test/trace/deny/explore towers, durable replay-resume, served over HTTP
- [x] Backend differential-parity suite (EIR VM vs interpreter); 5 higher-order builtin fixes

---

## v0.2.1 — Close the gaps

What's left to finish v0.2 properly.

### Module system in type checker & codegen
- [x] `[use ...]` resolved during type checking (cross-file inference)
- [x] `[pub ...]` visibility enforced in type checker
- [x] Multi-file compilation to WASM (module linking/bundling)

### Closure capture classification
- [x] Ref vs move capture analysis (integrate with ownership checker)

---

## v0.3 — Types get serious

### Traits / protocols
- [x] `[trait Display [fn display [self] → String]]`
- [x] `[impl Display Shape ...]`
- [x] Trait-based operator overloading (`Add`, `Eq`, `Ord`)
- [x] Trait bounds in type inference

### Advanced type features
- [x] Typed AST (separate from untyped — type checker produces typed tree)
- [x] `[sig]` assertions checked against inferred types
- [x] Row polymorphism for maps/records
- [x] Type error messages with source spans (integrate with codespan-reporting)

### Ownership improvements
- [x] Borrow inference per-parameter (read-only → immutable borrow, mutates → mutable borrow, escapes → move)
- [x] Copy types: primitives auto-copy, `[derive Copy [type ...]]`
- [x] Better error messages: what/why/how format with visual ownership diagrams

---

## v0.4 — Effects for real

### Full effect system
- [x] Effect inference: calling `IO.read-file` propagates `IO` to caller
- [x] Effect annotations checked: `/ {IO Fail}` verified against inferred set
- [x] `?` desugaring: `[expr]?` → match on Result, perform Fail on Err
- [x] Partial handling: handle some effects, pass others through

### Built-in effects
- [x] `IO`: file read/write, stdin/stdout, env vars (via WASI)
- [x] `Fail`: Result integration, `?` sugar
- [x] `Async`: placeholder runtime handler, mock handler for testing
- [x] Channels: `[let [tx rx] [channel]]`, `[send tx val]`, `[recv rx]`

### Physics type system (v0.4.22)
- [x] `Dimension` struct with SI exponents (mass, length, time, current, temperature)
- [x] `Type::Dim` variant in type system — compile-time dimensional analysis, zero runtime overhead
- [x] Literal unit suffixes: `5.0m`, `9.81kg`, `10kN` desugar to `[unit value :keyword]`
- [x] 30+ units with SI prefixes (m, km, cm, mm, s, ms, kg, g, N, kN, Pa, MPa, GPa, J, W, kW, Hz, A, mA, V, C, ohm, K)
- [x] 21 named quantities (Velocity, Force, Energy, Power, Pressure, Density, etc.)
- [x] No-dimensionless rule: `Dim÷Dim → Scalar` (not Float), explicit `magnitude` to exit
- [x] `Physics` effect (gravity, yield-strength, elastic-modulus, density, temperature, thermal-conductivity)
- [x] `Sim` effect (stress, deflection, natural-freq, thermal-field) — bridge point for phyz
- [x] Dimensional polymorphism: `[fn double [x] [* 2.0 x]]` works on any dimension
- [x] Physics-aware E0208 errors with named quantities and operation hints
- [x] 5 physics constants: `Const.c`, `Const.G`, `Const.h`, `Const.k-B`, `Const.e-charge`

---

## v0.5 — WASM gets real

### Expanded codegen
- [x] Closures (indirect calls + captured environments)
- [x] ADTs (tagged unions on the heap)
- [x] Pattern matching compilation (decision trees + br_table)
- [x] Persistent data structures (vec-new, vec-push, vec-get)
- [x] String operations beyond literals (str-len, str-concat, str-eq)
- [x] WASI integration: file I/O, args, env

### Runtime
- [x] `loon run` executes WASM via wasmtime (instead of interpreting)
- [x] `loon build --release` with tree-shaking
- [x] Target: hello world < 1KB, fib < 500 bytes

---

## v0.6 — Developer experience

### Error messages that teach
- [x] Three-part errors: what / why / how to fix
- [x] Visual ownership diagrams in error output
- [x] `[explain EXXXX]` interactive REPL tutorials (not just text)
- [x] Structured errors as data: `[catch-errors "[source]"]`

### LSP server
- [x] Go-to-definition
- [x] Type-on-hover (the invisible type system, made visible)
- [x] Autocomplete
- [x] Inline diagnostics
- [x] Inlay hints for inferred types (the three rendering modes from DESIGN.md)

### Formatter
- [x] `loon fmt` — deterministic structural formatting
- [x] No config, no debates — one true style

### Tree-sitter grammar
- [x] Syntax highlighting for editors

---

## v0.7 — Macros

### Hygienic macros
- [x] `[macro when [condition & body] ...]`
- [x] Quasiquoting: `` ` ``, `~`, `~@`
- [x] Hygiene by default (Scheme-style)
- [x] `[macro+ ...]` type-aware macros (run after type inference)

---

## v0.8 — Package Manager

### Phase 1 — Manifest, CLI, local deps (done)
- [x] `pkg.oo` manifest format (Loon data format, not TOML)
- [x] `loon new`, `loon init` — project scaffolding
- [x] `loon add`, `loon remove` — dependency management
- [x] Path dependencies: `{:path "../my-lib"}`
- [x] Version constraints: `^`, `~`, `>=`, `=`, `*`
- [x] Capability grants: `:grant #["Net" "IO"]`
- [x] `loon audit --capabilities` — report effect grants
- [x] `loon why <source>` — dependency trace
- [x] `loon search <query>` — search package index
- [x] `loon cache clean` — clear cache

### Phase 2 — Git, URLs, cache, lockfile (done)
- [x] Domain detection: `github.com/user/repo` recognized as remote
- [x] Git fetch: `git clone --depth 1` to temp dir
- [x] URL fetch: HTTP GET + tar.gz extraction (ureq + flate2 + tar)
- [x] Archive URL derivation for GitHub, GitLab, Codeberg
- [x] BLAKE3 content-addressed hashing
- [x] Cache at `~/.loon/cache/blake3/<hash>/`
- [x] `lock.oo` — lockfile in Loon data format
- [x] `loon add` auto-fetches and locks domain-qualified deps
- [x] `loon cache warm` — fetch all unfetched deps
- [x] Feature-gated: `pkg-fetch` (CLI only, not WASM)
- [x] Subpath support: `github.com/cam/std#http`

### Phase 3 — Resolution, registry, transitive deps (done)
- [x] Transitive dependency resolution (parse fetched pkg.oo, resolve recursively)
- [x] MVS (Minimum Version Selection) across the dep graph
- [x] `loon update` — re-resolve and update lock.oo
- [x] Package registry/index — built-in seed + fetchable remote indices
- [x] Custom indices via `:indices` in pkg.oo
- [x] `loon search` searches across builtin + custom indices
- [x] `loon why` traces transitive dependency chains

### Phase 4 — Publish, verify, audit, capability propagation (done)
- [x] `loon publish` — create tarball + hash for publishing
- [x] `loon verify` — verify cache integrity against lockfile hashes
- [x] `loon audit` — full audit: capabilities, transitive grants, cache integrity, lockfile status
- [x] Hash verification on load (verify cache integrity in `resolve_remote_dep`)
- [x] Capability propagation (transitive grant checking)

### File extension migration
- [x] `.oo` as primary file extension (from l-**oo**-n), `.loon` as fallback
- [x] `pkg.oo` / `lock.oo` manifest and lockfile (falls back to `.loon`)
- [x] `loon new` creates `.oo` files
- [x] Module resolution tries `.oo` first, falls back to `.loon`

### Future
- [ ] IPFS distribution

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
- [ ] `.oo.nb` — interleaved markdown and code cells
- [ ] Same type/ownership checking as regular files
- [ ] Compiles to WASM

---

## Non-goals for now

- Native (non-WASM) compilation target
- Backward compat with any existing LISP
- GUI framework (provide FFI, frameworks are packages)
- Full HM typing of escaping/answer-passing handlers (the runtime is multi-shot;
  the checker still types `resume : a -> a`, so the answer-passing style runs but
  does not yet type-check — see `samples/state.oo`)

---

*Loon: a LISP that flies.*
