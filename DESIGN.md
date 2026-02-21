# Loon Language Design Document

> A beautiful, ergonomic LISP with Rust-like safety and a decentralized package manager.
> Compiles to WebAssembly.

---

## 1. Philosophy & Goals

Loon is a LISP that steals the best ideas from Rust, Clojure, and Scheme — then makes them feel effortless.

**Core principles:**

- **Ergonomic LISP.** `[]` as primary s-expression delimiters. Square brackets are easier to type, easier to scan, and free up `()` for grouping and tuples.
- **Rust-level safety without Rust-level verbosity.** Ownership, borrowing, algebraic types — but inferred aggressively so you rarely write annotations.
- **WASM-first.** WebAssembly is the primary (and initially only) compilation target. WASI for system interfaces.
- **Content-addressed packages.** No central registry required. Packages are identified by the hash of their source. Immutable by construction.
- **Batteries considered.** The standard library is small but thoughtful. The package manager makes it trivial to pull in what you need.

**Non-goals:**

- Being a general-purpose systems language (use Rust)
- Backward compatibility with any existing LISP
- Supporting every platform natively (WASM is the abstraction layer)

---

## 2. Syntax

`[]` for s-expressions. `()` for grouping/tuples. `{}` for maps/records. Style B (indentation-aware sweet expressions) may be added later as a preprocessor.

```loon
[defn greet [name]
  [str "hello, " name "!"]]

[defn greet-all [names]
  [pipe names
    [map greet]
    [collect]]]

[defn main []
  [let users #["alice" "bob" "carol"]]
  [pipe [greet-all users]
    [each println]]]
```

Everything is explicit. Brackets everywhere. No type annotations — they're inferred and shown by the editor. Maximally homoiconic — what you see is the AST.

---

## 3. Type System

### The Invisible Type System

Loon's most radical design decision: **you don't write type annotations.** Ever.

Types are inferred by the compiler, stored alongside the content-addressed definition hash (Section 12K), and rendered by the editor as phantom text. The source file contains zero type syntax. The compiler knows every type. The editor shows you whatever you want to see. But the *code you write* is just logic.

```loon
; What you write:
[defn greet [name]
  [str "hello, " name]]

; What the editor renders (toggleable phantom text):
[defn greet [name ⸬String] ⸬String
  [str "hello, " name]]

; What the compiler stores with the definition hash:
;   greet : String → String
```

**Why this works for Loon (and wouldn't work for most languages):**
- Hindley-Milner inference is *complete* for the core language — every expression has a principal type
- LISP's structural simplicity (everything is an expression, uniform syntax) makes inference tractable even for complex programs
- Content-addressed definitions (Section 12K) mean the inferred types are stored permanently — they're not re-inferred on every build, they're part of the definition's identity
- The structural editor (Section 12N) renders types inline, on hover, or in a sidebar — you choose the visibility level

**When inference isn't enough:** the `[sig]` form lets you constrain a function's type. This is rare — you'd use it for polymorphic functions where you want to restrict the type, or for documentation at module boundaries.

```loon
[sig parse : String → Result Config ParseError]
[defn parse [raw]
  ...]
```

`[sig]` is an assertion, not an annotation. The compiler checks that the inferred type matches. If it doesn't, you get an error.

**Editor experience** — three rendering modes:

1. **Clean** (default) — no types shown. Just your code.
2. **Subtle** — types appear as dimmed phantom text after names, like VS Code inlay hints but pervasive.
3. **Full** — types rendered inline as if you'd written them. Useful for learning and code review.

Toggle with a keybinding. Hover any expression to see its type. Click a type to jump to its definition.

**Open question: does full inference scale?** Adding ownership, borrowing, and effect tracking on top of HM significantly complicates inference. Vanilla HM is decidable; HM + affine types + effects is an open research area. Our bet is that LISP's uniform syntax makes this more tractable than in languages with complex control flow. If the bet is wrong, we add `[sig]` requirements at module boundaries — it's easier to tighten than loosen.

### Primitives

```loon
42        ; i64 (default integer)
42i32     ; i32 (suffix for non-default)
3.14      ; f64 (default float)
3.14f32   ; f32
true      ; Bool
"hello"   ; String
:keyword  ; Keyword (interned symbol)
```

### Algebraic Data Types

Type *definitions* are the one place where types appear in source — because you're defining new types, not annotating existing values.

```loon
[type Option T
  [Some T]
  None]

[type Result T E
  [Ok T]
  [Err E]]

[type Shape
  [Circle f64]
  [Rect f64 f64]
  [Point]]
```

### Traits (Protocols)

Trait definitions also contain type information — they're contracts, and contracts need to be explicit.

```loon
[trait Display
  [fn display [self] → String]]

[trait Add T
  [type Output]
  [fn add [self other] → Self.Output]]
```

Implementations don't need type annotations — the trait provides them:

```loon
[impl Display Shape
  [fn display [self]
    [match self
      [Circle r]  => [str "circle(" r ")"]
      [Rect w h]  => [str "rect(" w "x" h ")"]
      Point       => "point"]]]
```

### No Null

There is no null, nil, or undefined. Use `Option` for values that might not exist. Use `Result` for operations that might fail. The type system enforces exhaustive handling.

---

## 4. Ownership & Borrowing

Loon adopts Rust's ownership model but infers more aggressively.

### Core Rules

1. Every value has exactly one owner.
2. When the owner goes out of scope, the value is dropped.
3. Values can be *moved* (ownership transferred) or *borrowed* (temporary access).
4. At any time: either one mutable reference OR any number of immutable references. Never both.

### Inference — Not Annotation

Ownership and borrowing follow the same philosophy as types: **inferred, not annotated.**

The compiler performs whole-function analysis. If a function only reads a parameter, it's automatically borrowed. If it stores it in a struct or returns it, it's moved. You never write `&` or `&mut` — the compiler figures it out.

```loon
[defn greet [name]
  [str "hello, " name]]

[let name "alice"]
[greet name]      ; compiler infers borrow — greet only reads name
[println name]    ; still valid — name wasn't moved
```

```loon
; Mutation — the compiler infers mutable borrow:
[defn add-world [s]
  [push! s " world"]]    ; push! requires mutation → compiler infers &mut

; Ownership transfer — the compiler infers move:
[defn take [s]
  [store-in-db s]]        ; s escapes into a struct → compiler infers move
```

### Lifetimes

Lifetimes exist in the type system but are always inferred. The compiler tracks them internally; you never write `'a`.

```loon
[defn first [s]
  [slice s 0 1]]
; Compiler infers: the output lifetime is tied to the input lifetime
```

In the rare case where lifetime inference is ambiguous, resolve it with `[sig]`:

```loon
[sig longest : &'a str → &'a str → &'a str]
[defn longest [a b]
  [if [> [len a] [len b]] a b]]
```

### Copy Types

Primitives (`i32`, `f64`, `Bool`, etc.) implement `Copy` — they're duplicated instead of moved. Small structs can opt in:

```loon
[#[derive Copy]
 type Point [x f64] [y f64]]
```

### Unsafe

For operations the compiler can't verify — raw memory access, certain FFI calls:

```loon
[unsafe
  [let ptr [alloc 1024]]
  [write-bytes ptr data]
  [dealloc ptr]]
```

`unsafe` blocks are checked, flagged by `pond audit`, and should be rare. The same semantics as Rust: you're telling the compiler "I've verified this is sound, trust me."

---

## 5. Core Data Structures

### The Hybrid Model

Loon has two families of data structures:

- **Persistent (immutable)** — Clojure-style structural sharing. Use reference counting internally. This is the default.
- **Owned (mutable)** — Rust-style single-owner, no RC overhead. Opt-in when you need performance.

The ownership model governs the *handles* to persistent data (who can read, who can drop the reference), while reference counting governs the *shared internal structure*. This is analogous to Rust's `Arc<Vec<T>>` — the `Arc` is refcounted, the `Vec` inside is owned.

### Persistent Vectors

```loon
[let v #[1 2 3 4 5]]
[let v2 [conj v 6]]         ; #[1 2 3 4 5 6] — v is unchanged
[nth v 0]                    ; 1
[len v]                      ; 5
```

`v` and `v2` share structure internally (refcounted tree nodes). Creating `v2` doesn't copy the whole vector.

### Persistent Maps

```loon
[let m {:name "loon" :version "0.1" :cool true}]
[get m :name]                ; "loon"
[let m2 [assoc m :version "0.2"]]
```

### Sets

```loon
[let s #{1 2 3}]
[contains? s 2]              ; true
[let s2 [conj s 4]]          ; #{1 2 3 4}
```

### Owned Mutable Variants

When you need mutation and zero RC overhead (hot loops, performance-critical code):

```loon
[let mut v [mut-vec 1 2 3]]
[push! v 4]                  ; mutates in place, no RC
```

Owned collections follow standard ownership rules — single owner, no sharing. The `!` suffix on `push!` is a convention for mutating functions.

### Strings

UTF-8 by default. Same owned/borrowed distinction as Rust, but inferred:

```loon
[let s "hello"]                ; string literal — borrowed from static data
[let s2 [String.from "hello"]] ; heap-allocated, owned
```

---

## 6. Functions & Pattern Matching

### Function Definition

```loon
[defn add [x y]
  [+ x y]]
; Compiler infers: add : i64 → i64 → i64
```

### Multi-Arity

```loon
[defn greet
  ([name]
    [str "hello, " name])
  ([greeting name]
    [str greeting ", " name])]

[greet "world"]              ; "hello, world"
[greet "hey" "world"]        ; "hey, world"
```

### Pattern Matching

```loon
[match shape
  [Circle r]    => [* 3.14159 r r]
  [Rect w h]    => [* w h]
  Point         => 0.0]

; With guards:
[match n
  0             => "zero"
  n [when [> n 0]] => "positive"
  _             => "negative"]

; Destructuring:
[let [x y] [get-point]]
[let {name age} user]
```

### Closures

```loon
[fn [x] [+ x 1]]            ; anonymous function
[let inc [fn [x] [+ x 1]]]
[map inc #[1 2 3]]           ; #[2 3 4]
```

### Pipe Operator

```loon
[pipe #[1 2 3 4 5]
  [map [fn [x] [* x x]]]
  [filter [fn [x] [> x 5]]]
  [collect]]
; #[9 16 25]
```

---

## 7. Modules & Visibility

### File = Module

Every `.oo` file is a module (`.loon` also supported as fallback). The module name matches the file path relative to `src/`:

```
src/
  main.oo            ; module: main
  http/
    server.oo        ; module: http.server
    client.oo        ; module: http.client
```

### Visibility

Everything is private by default. Use `pub` to export:

```loon
[pub defn serve [port]
  ...]

[pub type Config
  [port u16]
  [host String]]

[defn parse-header [raw]    ; private
  ...]
```

### Imports

```loon
[use std.io]                         ; use as std.io.read, etc.
[use std.io :as io]                  ; alias: io.read, etc.
[use std.collections {HashMap HashSet}] ; import specific items
```

---

## 8. Algebraic Effects

This is Loon's unified model for side effects. Async, errors, IO, state, logging — all expressed as effects.

### What's an Effect?

An algebraic effect is a declared side effect that a function *performs* and a caller *handles*. Think of it as a resumable exception with a type system.

```loon
; Declare effects:
[effect IO
  [fn read-file [path] → String]
  [fn write-file [path content]]]

[effect Fail
  [fn fail [msg] → !]]
```

### Using Effects

Effects appear after `/` in function signatures — this IS visible in source, because effects are contracts:

```loon
[defn load-config [path] / {IO Fail}
  [let raw [IO.read-file path]]
  [if [empty? raw]
    [Fail.fail "config file is empty"]]
  [parse-toml raw]]
```

### Handling Effects

Callers handle effects at the call site:

```loon
[handle [load-config "app.toml"]
  [IO.read-file path] => [resume [mock-fs.read path]]
  [IO.write-file _ _] => [resume]
  [Fail.fail msg]     => [Config.default]]
```

### Error Handling — `Result` and `?` as Sugar

`Result` is Loon's conventional error type. The `?` operator is sugar for performing and handling the `Fail` effect:

```loon
; These two are equivalent:
[defn load-config [path] / {Fail}
  [let raw [read-file path]?]       ; ? performs Fail on Err
  [let config [parse-toml raw]?]
  [Ok config]]

; Desugars to:
[defn load-config [path] / {Fail}
  [let raw [match [read-file path]
    [Ok v]  => v
    [Err e] => [Fail.fail e]]]
  ...]
```

`?` is the common case. `[handle ...]` is the general case. They compose.

### Async — Also Just an Effect

```loon
[effect Async
  [fn await [future] → T]]

; [async defn ...] is sugar for a function with the Async effect:
[defn fetch-data [url] / {Async IO}
  [let response [Async.await [http.get url]]]
  response.body]

; The runtime provides the Async handler. You can provide your own for testing:
[handle [fetch-data "https://example.com"]
  [Async.await f] => [resume [resolve-immediately f]]]
```

### Channels and Concurrency

```loon
[let [tx rx] [channel]]

[spawn [fn []
  [send tx "hello from task"]]]

[let msg [Async.await [recv rx]]]
[println msg]
```

The ownership model prevents data races: you can't share mutable references across tasks. If you need shared state, use channels or atomics.

### Effects + Ownership

Algebraic effects involve capturing continuations. Continuations can duplicate references that are supposed to be unique. Loon handles this the same way Koka does: **effect handlers consume their continuation linearly.** A handler must call `[resume ...]` exactly once (or zero times, if it doesn't resume). The compiler enforces this. Multi-shot continuations require explicit cloning, which only works for `Copy` types.

### Why Effects Are Legendary

- **Testing without mocks.** Handle the `IO` effect with test data — no DI frameworks, no mock libraries.
- **Composable.** A function with `{IO Fail Log}` effects can be partially handled — handle `Log`, pass the rest through.
- **Replaces 5 features with 1.** Exceptions, async/await, generators, dependency injection, algebraic state — all expressible as effects.
- **Informed by research.** Draws from Koka, Eff, and OCaml 5. No mainstream language has shipped this yet.

---

## 9. Macros

### Hygienic Macros

Loon macros are hygienic by default (like Scheme's `syntax-rules`). They operate on syntax, not text.

```loon
[defmacro when [condition & body]
  `[if ~condition
     [do ~@body]
     nil]]

[when [> x 10]
  [println "big"]
  [log "x was big"]]

; Expands to:
[if [> x 10]
  [do
    [println "big"]
    [log "x was big"]]
  nil]
```

### Quasi-Quoting

- `` ` `` — quote (don't evaluate)
- `~` — unquote (evaluate this part)
- `~@` — splice-unquote (evaluate and spread)

### Type-Aware Macros

Unlike traditional LISPs, Loon macros can optionally run *after* type inference, giving them access to type information.

```loon
[defmacro+ derive-serialize [T]    ; the + means "type-aware"
  [let fields [type-fields T]]
  `[impl Serialize ~T
     [fn serialize [self writer]
       ~@[map [fn [f]
                `[serialize-field writer ~(field-name f) self.~(field-name f)]]
              fields]]]]
```

This enables zero-cost derive macros (`[#[derive Debug Serialize Eq]]`), compile-time ORMs, and generic serialization — all implemented in Loon itself. Still hygienic.

---

## 10. Testing

### Test Functions

```loon
[test defn test-greet []
  [assert-eq [greet "world"] "hello, world!"]]

[test defn test-fib []
  [assert-eq [fib 0] 0]
  [assert-eq [fib 10] 55]]
```

`[test defn ...]` marks a function as a test. It's a regular function — no special syntax, no test framework to import.

### Running Tests

```sh
$ loon test
  Running 4 tests...
  ✓ test-greet .................. 0.1ms
  ✓ test-fib .................... 0.2ms
  ✓ test-parse .................. 0.3ms
  ✗ test-edge-case .............. 0.1ms
    assert-eq failed:
      expected: 42
      actual:   41
      at: src/math.oo:28
  3 passed, 1 failed (0.7ms)
```

### Property-Based Testing

```loon
[test defn test-sort-idempotent []
  [check [fn [xs]            ; check generates random inputs
    [assert-eq [sort [sort xs]] [sort xs]]]]]

[test defn test-reverse-reverse []
  [check [fn [xs]
    [assert-eq [reverse [reverse xs]] xs]]]]
```

### Testing Effects

Effects make testing trivial — handle IO with test data, no mocks needed:

```loon
[test defn test-load-config []
  [let result
    [handle [load-config "app.toml"]
      [IO.read-file _] => [resume "name = \"test\""]
      [Fail.fail msg]  => [Err msg]]]
  [assert-eq result.name "test"]]
```

---

## 11. WASM Target

### Compilation Model

Loon has two execution modes:

- **Compiled** — `loon build` produces WASM bytecode via ahead-of-time compilation. This is the shipping format.
- **Interpreted** — the REPL uses a tree-walking interpreter over the typed AST. No WASM compilation step. This enables instant feedback, time travel, and hot reload.

The same type checker and ownership checker run in both modes. The interpreter enforces ownership rules at runtime (consuming values on move, tracking borrows). The compiler enforces them statically.

### Build Commands

```sh
loon build              # produces target/main.wasm
loon build --release    # optimized, tree-shaken
loon run                # build + execute via Wasmtime
```

### WASI

System interfaces via WASI:

```loon
[use wasi.fs {read-file write-file}]
[use wasi.env {args vars}]

[defn main []
  [let name [nth [args] 1 "world"]]
  [println [str "hello, " name]]]
```

### FFI / Extern

Extern declarations are where `[sig]` is required — you can't infer types across language boundaries:

```loon
[extern "env"
  [sig log : &str → void]
  [fn log [msg]]]
```

Export Loon functions to the host:

```loon
[sig handle-request : Request → Response]
[pub extern defn handle-request [req]
  [Response.ok "hello from loon"]]
```

### Browser / JS Interop

DOM and browser APIs are accessed via extern declarations, same as any WASM host:

```loon
[extern "env"
  [sig query-selector : &str → Element]
  [fn query-selector [sel]]

  [sig set-text-content : Element → &str → void]
  [fn set-text-content [el text]]]
```

Loon doesn't provide a built-in browser framework — it provides the FFI. Frameworks are packages.

### Component Model

Loon supports the WASM Component Model for composing modules across languages:

```loon
[import "wasi:http/handler" {handle}]
[export "wasi:http/handler"
  [fn handle [req] ...]]
```

---

## 12. What Makes Loon Legendary

This section is the soul of the design — the features that make people say "holy shit" and rewrite their side projects in Loon overnight.

### A. The REPL Is a Time Machine

The Loon REPL isn't a read-eval-print loop. It's a persistent, image-based development environment — like Smalltalk, but with types and ownership.

```loon
loon> [let x 42]
loon> [let y [+ x 8]]
loon> y
50
loon> [rewind 2]          ; undo last 2 evaluations
loon> x
42
loon> y
; error: unbound symbol 'y'
loon> [fork]               ; branch this session
loon (fork-1)> [let y 999]
loon (fork-1)> [exit-fork]
loon> y
; error: unbound symbol 'y'  (fork was discarded)
```

**Key features:**
- **Persistent state.** Your REPL session is serializable. Close your laptop, reopen, pick up exactly where you left off.
- **Time travel.** `[rewind n]` steps back n evaluations. `[snapshot]` saves a named checkpoint.
- **Forking.** `[fork]` branches your session for experimentation. Keep or discard.
- **Hot reload.** Redefine functions, types, even trait implementations mid-session. The REPL re-typechecks incrementally.
- **The REPL is the debugger.** Set breakpoints with `[break-at module.fn]`. Inspect any value. Step through execution.

#### The REPL Speaks English

Input that starts with `[` or is a bare symbol is code. Everything else is natural language. No mode-switching, no prefix — just type.

```loon
loon> [+ 1 2]
3

loon> what does the greet function do?
  greet takes a String and returns "hello, {name}!"
  Defined at src/main.oo:3. Last modified 2 minutes ago.

loon> add a test for greet that checks empty strings
  ✓ Generated test:
  [test defn test-greet-empty []
    [assert-eq [greet ""] "hello, !"]]
  Apply? [y/n/edit]

loon> refactor fib to use tail recursion
  ✓ Proposed change to fib:
  [defn fib [n]
    [fib-iter n 0 1]]
  [defn fib-iter [n a b]
    [match n
      0 => a
      n => [fib-iter [- n 1] b [+ a b]]]]
  Apply? [y/n/edit]

loon> show me everything that depends on Config
  3 functions reference Config:
    load-config  (src/config.oo:12) — constructs Config
    validate     (src/config.oo:28) — borrows Config
    main         (src/main.oo:5)    — owns Config
```

The AI operates on the typed AST, the effect system, and the dependency graph. It can't propose code that doesn't type-check, violates ownership, or exceeds capabilities. The language *constrains* the AI's output to be correct.

### B. Provably Minimal Binaries

Loon doesn't just tree-shake functions. It tree-shakes *types*.

The ownership model tells the compiler exactly what's alive and what's dead. If no live code path owns a value of type `T`, then `T`'s methods, trait impls, and vtables don't ship.

**Targets:**
- Hello world WASM binary: **< 1KB**
- HTTP handler with routing: **< 50KB**
- Full CLI tool with argument parsing: **< 100KB**

```sh
$ loon build --release
   Compiled main.wasm (847 bytes)
   Type-level DCE removed 14 types, 89 functions
```

### C. Error Messages That Teach

Every Loon error includes three things: **what** went wrong, **why** it's wrong, and **how** to fix it.

```
error[E0312]: cannot borrow `data` as mutable — already borrowed as immutable

   ┌─ src/main.oo:14:5
   │
12 │  [let view [slice data 0 3]]
   │            ───── immutable borrow occurs here
13 │  [println view]
14 │  [push! data 42]
   │  ^^^^^^^^^^^^^^ mutable borrow attempted here
15 │  [println view]
   │           ──── immutable borrow still in use here

   why: Loon prevents reading and writing the same data simultaneously
        to eliminate data races and dangling references.

   fix: Move the mutable operation before or after the immutable borrow:

   │  [push! data 42]        ; ← move this up
   │  [let view [slice data 0 3]]
   │  [println view]
   │  [println view]

   help: [explain E0312] for an interactive tutorial on borrowing
```

**Error features:**
- **Visual ownership diagrams** showing who owns what and where the conflict is
- **`[explain E0312]`** opens an interactive REPL-based tutorial for that error class
- **Errors are data.** `[catch-errors [compile "src/main.oo"]]` returns structured error values

### D. Macros That See Types

(Full specification in Section 9.)

Loon's `[defmacro+ ...]` macros run *after* type inference, with access to the type environment. This enables:

- **Zero-cost derive macros** — `[#[derive Debug Serialize Eq]]`, implemented in Loon itself
- **Compile-time ORMs** — generate type-safe queries from struct definitions
- **Generic serialization** — branch on types, generate optimal code per type

Still hygienic — type-awareness doesn't mean type-unsafety.

### E. Capability-Based Security

Every Loon module declares what it needs:

```loon
[capabilities
  :net                    ; network access
  :fs.read ["./data/*"]  ; read files in ./data/
  :fs.write ["./out/*"]  ; write files in ./out/
  :env ["API_KEY"]]      ; read one env var
```

Callers grant capabilities explicitly:

```loon
[use untrusted-dep
  :grant [:net :fs.read ["./cache/*"]]]
```

**Properties:**
- A dependency can't phone home unless you grant `:net`
- Capabilities compose — if you don't grant `:net` to a dep, none of *its* deps get `:net` either
- Pairs perfectly with WASM's sandboxing model
- `pond audit --capabilities` reports what every dep requires

### F. Content-Addressed Definitions

Stolen from Unison — and extended.

Every function and type is identified by the hash of its *content* (AST after desugaring). Names are metadata, not identity.

```loon
loon> [hash greet]
sha256:7f3a...

loon> [rename greet say-hello]
; same hash — renaming doesn't change identity

loon> [history greet]
  sha256:7f3a... (current)  — added "!" to greeting
  sha256:2b1c... (2 edits ago) — initial implementation
```

**Consequences:**
- **Merge conflicts on renames are impossible.** Both names point to the same hash.
- **Refactoring is free.** Move a function between modules? The hash doesn't change.
- **Dead code is provable.** If no live hash references your definition's hash, it's dead — mathematically.
- **Pairs with Pond.** Packages are content-addressed. Definitions are content-addressed. Hashes all the way down.
- **Types are stored with the hash.** This is what makes the invisible type system work — inferred types are part of the definition's identity, not re-inferred on every build.

### G. Structural Editing & Tooling

Since Loon is a LISP, the AST *is* the syntax. This makes tooling trivially derivable:

- **Formatter** — structurally format any Loon code (no ambiguity, no style debates)
- **Linter** — pattern-match on the AST directly
- **LSP** — autocomplete, go-to-definition, type-on-hover — all from the same data structure
- **Tree-sitter grammar** — near-trivial (it's just nested brackets)
- **AI-friendly** — LLMs generate Loon structurally. No operator precedence, no semicolons, no whitespace ambiguity.

The blessed editing experience is structural: the cursor moves between AST nodes, not characters.

```
; Cursor is on the [+ x 1] node:
[defn inc [x] «[+ x 1]»]

; Press "w" to wrap in a new form:
[defn inc [x] [«▮» [+ x 1]]]

; Type "if [> x 0]":
[defn inc [x] [if [> x 0] [+ x 1] «▮»]]
```

Plain text editing always works. Structural editing is the blessed path, not a cage.

```loon
; Code is data:
[let code [quote [+ 1 2]]]
[eval code]                    ; 3
```

`[quote]` and `[eval]` aren't just features — they're the extension model.

### H. Incremental Computation

Loon's runtime includes a built-in incremental computation engine, inspired by Salsa (the engine inside rust-analyzer).

```loon
[memo defn parse [source]
  [parser.parse source]]

[memo defn typecheck [ast]
  [checker.check ast]]

[memo defn compile [source]
  [pipe source [parse] [typecheck] [codegen]]]
```

When `source` changes, only the affected stages re-run. The runtime tracks `[memo]` dependencies automatically.

**Applications:**
- **The Loon compiler itself** uses this — edit one function, only that function re-compiles.
- **Reactive UIs.** `[memo]` functions + a render loop = a reactive framework with zero library code.
- **Data pipelines.** Change one input, only downstream transformations re-run.

### I. Provenance Tracking

Every value in Loon can carry metadata about its origin — where it was created, how it was transformed.

```loon
[let name [read-input "What's your name?"]]
[let greeting [str "hello, " name]]

[provenance greeting]
; => #[
;   {origin: "stdin" fn: read-input}
;   {transform: str fn: main}
; ]
```

**Cost model:** Provenance is tracked via compile-time instrumentation, not runtime tagging. The compiler inserts tracking code only for values that flow into a `[provenance ...]` call or are marked with `[#[track] ...]`. Untracked code paths have zero overhead. Tracked paths pay proportional to the number of transformations — comparable to structured logging.

**Applications:**
- **Debugging.** "Where did this value come from?" — answered instantly.
- **Security.** Track tainted data from user input through your program.
- **Compliance.** "Show me every value derived from PII."

### J. Transparent Persistence

Loon's persistent data structures can be transparently backed by durable storage.

```loon
[let db [Store.open "./app.db"]]

[let db [assoc db :users #[
  {:name "alice" :role :admin}
  {:name "bob" :role :user}]]]

; Survives process restarts:
[let db [Store.open "./app.db"]]
[get db :users]  ; still there
```

**Key properties:**
- **No ORM.** Your data structures *are* your database.
- **Immutable history.** `[Store.at db timestamp]` returns the database as it was at that point.
- **Transactional.** `[transact db ...]` blocks are atomic.
- **Scales down.** Small projects: a file with superpowers. Large projects: a proper embedded database.

### K. Notebooks as First-Class Programs

`.oo` files are programs. `.oo.nb` files are notebooks — interleaved Markdown and code cells:

````loon-nb
# Data Analysis

```loon
[use std.csv {read-csv}]
[let data [read-csv "./sales.csv"]?]
[println [str "Mean: $" [mean [map data :revenue]]]]
```
````

Notebooks compile to the same WASM as regular files. Type-checked, ownership-checked, no second-class citizens.

### L. Emotional Design

```
$ loon new my-project
  ✓ Created my-project/
  ✓ Created my-project/src/main.oo
  ✓ Created my-project/pkg.oo
  → cd my-project && loon run

$ loon run
  Compiling my-project (847 bytes)
  hello, world!
```

- Colors, Unicode box-drawing, progress spinners
- `loon new` creates exactly 2 files — no boilerplate
- The docs site is generated from `.oo.nb` notebooks, dogfooding the whole stack

### M. First-Class LLM & Agentic Programming

Loon is designed from the ground up to be the best language for writing AI-powered programs — and the best language for AI to write programs *in*.

#### `[ai ...]` — Compile-Time Code Generation

```loon
[sig celsius-to-fahrenheit : f64 → f64]
[ai defn celsius-to-fahrenheit [c]
  "Convert Celsius to Fahrenheit"]
```

The compiler sends the sig + docstring to an LLM, receives an implementation, type-checks it, and compiles it. If it doesn't type-check, retries up to 3x then errors.

```sh
$ loon build
  [ai] Generating celsius-to-fahrenheit... ✓ (type-checked, 1 attempt)
  Compiled main.wasm (1.2KB)
```

**Crucially:** `[ai ...]` blocks are **verified by the compiler**. The LLM proposes, the type system disposes.

Generated code is cached by prompt hash + model version. `loon build --offline` uses only cached generations. **Reproducible builds require `--offline`** — the `loon.lock` file pins cached AI generations alongside dependency hashes.

#### `[agent ...]` — Agent Loops as a Primitive

```loon
[defn research-agent [question] / {AI Net}
  [agent
    :system "You are a research assistant."
    :tools [web-search summarize]
    :max-turns 10
    :prompt question]]

; #[tool] auto-generates a JSON schema from the inferred type:
[#[tool "Search the web"]
 defn web-search [query] / {Net}
  [http.get [str "https://api.search.io?q=" [url-encode query]]]]
```

**Agent features:**
- **Tool type safety.** The type system *is* the tool specification. No manual schemas.
- **Observability.** `[agent-trace result]` returns the full chain of reasoning and tool calls.
- **Deterministic replay.** `[replay trace]` re-executes with same tool responses but different prompt.
- **Composable.** Agents call agents. Orchestrators delegate to specialists.

#### Structured Output — Types as Schemas

```loon
[type MovieReview
  [title String]
  [rating f64]
  [pros [Vec String]]
  [cons [Vec String]]
  [summary String]]

[let review [ai.extract MovieReview "Review the movie Dune Part 2"]]
review.rating  ; 8.5 — a real f64, not a string
```

Loon types map cleanly to JSON Schema. The compiler generates schemas from types automatically.

#### Semantic Functions

```loon
[sig classify-sentiment : String → Sentiment]
[semantic defn classify-sentiment [text]
  "Classify the sentiment of the given text."]

; Use like any other function:
[pipe [read-file "feedback.txt"]?
  [lines]
  [map [fn [line] {:text line :sentiment [classify-sentiment line]}]]
  [filter [fn [r] [= r.sentiment Sentiment.Negative]]]]
```

`[semantic defn ...]` declares a function whose implementation is a natural language prompt. The return type constrains the LLM's output. Marked with the `AI` effect — you always know what's hitting an LLM.

#### Sandboxed Code Execution

The killer combination: LLMs generate Loon code + capability system sandboxes it + WASM isolates it.

```loon
[let code [ai.complete "Write a function that sorts numbers"]]
[let ast [parse code]?]
[let checked [capability-check ast #{}]]    ; grant NO capabilities
[let result [eval-sandboxed checked #[3 1 4 1 5]]]
; => #[1 1 3 4 5]
```

The language itself provides the guardrails. No separate sandbox. The type system and capability system *are* the sandbox.

#### Model Configuration

```toml
# loon.toml
[ai]
default-model = "claude-sonnet"
compile-model = "claude-haiku"     # fast model for [ai defn] blocks
agent-model = "claude-opus"        # capable model for [agent] blocks
cache = true
offline = false
```

---

## 13. Decentralized Package Manager ("Pond")

### Manifest: `loon.toml`

```toml
[package]
name = "my-app"
version = "0.1.0"

[dependencies]
http = { hash = "sha256:a1b2c3d4...", source = "ipfs" }
json = { hash = "sha256:e5f6g7h8..." }

[capabilities]
net = true
fs.read = ["./data/*"]
```

`loon.toml` is the canonical location for dependencies. No in-source `[dep ...]` form — dependencies are configuration, not code.

### Content-Addressed Identity

A package's identity is the hash of its source tree. Not a name, not a version — the hash.

- Names are vanity — convenient but not authoritative
- The hash is the truth — you always get exactly what you asked for
- No squatting, no typosquatting, no supply chain attacks via name confusion

### Distribution

1. **IPFS** (default) — fully decentralized, anyone can pin
2. **HTTP** — any server can host packages at their hash
3. **Git** — pin to a specific commit hash
4. **Pond Index** — optional discovery layer, maps names → hashes

### Commands

```sh
pond add io.github.alice/http     # resolve name → hash, add to loon.toml
pond audit                        # type-check all deps against their interfaces
pond audit --capabilities         # report what each dep requires
pond verify                       # verify all dep hashes match content
pond publish                      # hash source, push to IPFS + index
```

### Reproducible Builds

Deps are hash-pinned. WASM is a deterministic target. AI-generated code is cached and pinned in `loon.lock`. Same source + same lock file = same binary, always. `loon build --offline` guarantees full reproducibility.

---

## 14. Example Programs

### Hello World

```loon
[defn main []
  [println "hello, world!"]]
```

### Fibonacci

```loon
[defn fib [n]
  [match n
    0 => 0
    1 => 1
    n => [+ [fib [- n 1]] [fib [- n 2]]]]]

[defn main []
  [pipe [range 0 10]
    [map fib]
    [each [fn [x] [println x]]]]]
```

### HTTP Server

```loon
[use std.http {Server Response}]

[defn handle [req]
  [match req.path
    "/"      => [Response.ok "welcome to loon!"]
    "/ping"  => [Response.ok "pong"]
    _        => [Response.not-found "404"]]]

[defn main [] / {Async IO}
  [let server [Server.bind "0.0.0.0:8080"]]
  [println "listening on :8080"]
  [Async.await [server.serve handle]]]
```

### CLI Tool: Word Counter

```loon
[use std.io {stdin read-to-string}]
[use std.collections {HashMap}]

[defn count-words [text]
  [pipe [split text " \n\t"]
    [filter [fn [w] [not [empty? w]]]]
    [fold [HashMap.new] [fn [acc word]
      [update acc word [fn [n] [+ [or n 0] 1]]]]]]]

[defn main [] / {IO Fail}
  [let text [read-to-string stdin]?]
  [let counts [count-words text]]
  [pipe [entries counts]
    [sort-by [fn [[_ n]] n] :desc]
    [take 10]
    [each [fn [[word n]]
      [println [str word ": " n]]]]]]
```

---

## 15. Syntax Comparison

### Fibonacci — Four Languages

**Loon:**
```loon
[defn fib [n]
  [match n
    0 => 0
    1 => 1
    n => [+ [fib [- n 1]] [fib [- n 2]]]]]
```

**Clojure:**
```clojure
(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))
```

**Rust:**
```rust
fn fib(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        n => fib(n - 1) + fib(n - 2),
    }
}
```

### HTTP Handler — Loon vs. Rust

**Loon:**
```loon
[defn handle [req]
  [match req.method
    :GET  => [Response.ok [get-page req.path]]
    :POST => [Response.ok [create-item [req.json]?]]
    _     => [Response.method-not-allowed]]]
```

**Rust (axum):**
```rust
async fn handle(req: Request) -> Response {
    match req.method() {
        Method::GET => Response::ok(get_page(req.path())),
        Method::POST => Response::ok(create_item(req.json().await?)),
        _ => Response::method_not_allowed(),
    }
}
```

Loon is radically more concise — zero type annotations — while retaining the same type safety.

---

## 16. Rust Safety Feature Parity

| Rust Feature | Loon Equivalent |
|---|---|
| Ownership | Same model, fully inferred |
| Borrowing & lifetimes | Same model, fully inferred, `[sig]` for rare ambiguity |
| No null | `Option` / `Result` only |
| Pattern matching exhaustiveness | Enforced by compiler |
| `Send` / `Sync` | Inferred from ownership |
| No data races | Ownership prevents shared mutable state |
| `unsafe` | `[unsafe ...]` blocks, same semantics |
| Algebraic types | `[type ...]` with full pattern matching |
| Traits | `[trait ...]` / `[impl ...]` |
| `Result` / `?` | Same (sugar for `Fail` effect) |
| `Drop` | `[trait Drop [fn drop [self]]]` |

---

## 17. Next Steps

1. **Formal grammar** — PEG or EBNF for the bracket syntax
2. **Rust implementation scaffold** — lexer, parser, AST types
3. **Type checker** — Hindley-Milner inference engine
4. **Effect checker** — effect inference and handler verification
5. **Ownership checker** — borrow checker with aggressive inference
6. **Tree-walking interpreter** — for the REPL
7. **WASM codegen** — emit WASM bytecode
8. **Pond** — package manager MVP (local-only first, then IPFS)

---

*Loon: a LISP that flies.*
