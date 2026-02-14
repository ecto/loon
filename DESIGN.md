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

## 2. Syntax Exploration

Loon's syntax is still under exploration. Two candidates are presented below using the same program — a function that greets a list of users and returns the results.

### Style A: Classic S-Expression with `[]`

```loon
[defn greet [name : String] : String
  [str "hello, " name "!"]]

[defn greet-all [names : [Vec String]] : [Vec String]
  [|> names
    [map greet]
    [collect]]]

[defn main []
  [let users #["alice" "bob" "carol"]]
  [|> [greet-all users]
    [each println]]]
```

Everything is explicit. Brackets everywhere. Maximally homoiconic — what you see is the AST.

### Style B: Indentation-Aware (Sweet Expressions)

```loon
defn greet [name : String] : String
  str "hello, " name "!"

defn greet-all [names : [Vec String]] : [Vec String]
  |> names
    map greet
    collect

defn main []
  let users #["alice" "bob" "carol"]
  |> greet-all users
    each println
```

Top-level forms use indentation instead of wrapping brackets. Inner expressions still use `[]` when needed for clarity. This is the "Python-ized LISP" approach — less visual noise, same semantics.

### Comparison

| Property | Style A (brackets) | Style B (sweet) |
|---|---|---|
| Homoiconicity | Perfect — AST = syntax | Slightly obscured by indentation rules |
| Macro-friendliness | Excellent | Requires indent-aware quasi-quoting |
| Readability for newcomers | Moderate (bracket fatigue) | High (familiar to Python/Haskell users) |
| Tooling complexity | Low | Medium (whitespace-sensitive parsing) |
| Copy-paste safety | High | Low (indentation can break) |

**Recommendation:** Start with Style A for the initial implementation. It's simpler to parse, simpler to macro-expand, and simpler to get right. Style B can be added as syntactic sugar later — a preprocessor that emits Style A.

---

## 3. Type System

Loon uses Hindley-Milner type inference with algebraic data types. You *can* annotate everything, but you almost never *need* to.

### Primitives

```loon
42        ; i64 (default integer)
42i32     ; i32
3.14      ; f64 (default float)
3.14f32   ; f32
true      ; Bool
"hello"   ; String
:keyword  ; Keyword (interned symbol)
```

### Algebraic Data Types

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

```loon
[trait Display
  [fn display [&self] : String]]

[trait Add T
  [type Output]
  [fn add [self other : T] : Self.Output]]

[impl Display Shape
  [fn display [&self] : String
    [match self
      [Circle r]  => [str "circle(" r ")"]
      [Rect w h]  => [str "rect(" w "x" h ")"]
      Point       => "point"]]]
```

### Type Aliases

```loon
[type-alias Name String]
[type-alias Pair T [T T]]
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
4. At any time: either one `&mut` reference OR any number of `&` references. Never both.

### Syntax

```loon
[defn take-ownership [s : String]    ; moves s into this function
  [println s]]

[defn borrow [s : &String]           ; borrows s immutably
  [println s]]

[defn mutate [s : &mut String]       ; borrows s mutably
  [push! s " world"]]
```

### Aggressive Inference

Unlike Rust, Loon infers borrow vs. move from usage:

```loon
[defn greet [name : String] : String
  [str "hello, " name]]

[let name "alice"]
[greet name]        ; compiler infers &String since greet only reads name
[println name]      ; still valid — name wasn't moved
```

The compiler performs whole-function analysis. If a function only reads a parameter, it's automatically borrowed. If it stores it in a struct or returns it, it's moved. You can always override with explicit annotations.

### Lifetimes

Lifetimes exist in the type system but are almost always elided:

```loon
; Explicit (rarely needed):
[defn first ['a] [s : &'a String] : &'a str
  [slice s 0 1]]

; Elided (the common case — compiler figures it out):
[defn first [s : &String] : &str
  [slice s 0 1]]
```

### Copy Types

Primitives (`i32`, `f64`, `Bool`, etc.) implement `Copy` — they're duplicated instead of moved. Small structs can opt in:

```loon
[#[derive Copy]
 type Point [x : f64] [y : f64]]
```

### Open Question

How much can the compiler infer before the programmer gets confused? Rust's explicitness is a feature for large codebases. Loon's bet is that LISP's structural simplicity (everything is an expression, no complex control flow) makes deeper inference tractable. If this proves wrong, we add more required annotations — it's easier to loosen than tighten.

---

## 5. Core Data Structures

### Vectors (Persistent, Immutable by Default)

```loon
[let v #[1 2 3 4 5]]
[let v2 [conj v 6]]         ; #[1 2 3 4 5 6] — v is unchanged
[nth v 0]                    ; 1
[len v]                      ; 5
```

### Maps (Persistent, Immutable by Default)

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

### Mutable Variants

When you need mutation (and you've got unique ownership):

```loon
[let mut v [mut-vec 1 2 3]]
[push! v 4]                  ; mutates in place
```

The `!` suffix is a convention for mutating functions. The type system enforces that `push!` requires `&mut`.

### Strings

UTF-8 by default. Strings are owned (`String`) or borrowed (`&str`), same as Rust:

```loon
[let s "hello"]              ; &str (string slice, borrowed from static data)
[let s2 [String.from "hello"]] ; String (owned, heap-allocated)
```

---

## 6. Functions & Pattern Matching

### Function Definition

```loon
[defn add [x : i64 y : i64] : i64
  [+ x y]]

; With type inference (types elided):
[defn add [x y]
  [+ x y]]
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

The pipe operator threads data through a sequence of transformations:

```loon
[|> #[1 2 3 4 5]
  [map [fn [x] [* x x]]]
  [filter [fn [x] [> x 5]]]
  [collect]]
; #[9 16 25]
```

---

## 7. Modules & Visibility

### File = Module

Every `.loon` file is a module. The module name matches the file path relative to `src/`:

```
src/
  main.loon          ; module: main
  http/
    server.loon      ; module: http.server
    client.loon      ; module: http.client
```

### Visibility

Everything is private by default. Use `pub` to export:

```loon
[pub defn serve [port : u16]
  ...]

[pub type Config
  [port : u16]
  [host : String]]

; Private helper — not exported
[defn parse-header [raw : &str] : Header
  ...]
```

### Imports

```loon
[use std.io]                         ; use as std.io.read, etc.
[use std.io :as io]                  ; alias: io.read, etc.
[use std.collections {HashMap HashSet}] ; import specific items
[use std.io {read write :as w}]      ; mix and match
```

---

## 8. Error Handling

### Result Type

Loon uses `Result` for all fallible operations. No exceptions.

```loon
[defn read-file [path : &str] : [Result String IoError]
  ...]

[match [read-file "config.toml"]
  [Ok content]  => [parse content]
  [Err e]       => [println "failed: " e]]
```

### The `?` Operator

Propagates errors up the call stack (like Rust):

```loon
[defn load-config [path : &str] : [Result Config AppError]
  [let raw [read-file path]?]
  [let config [parse-toml raw]?]
  [Ok config]]
```

### `try` Blocks

Group fallible operations:

```loon
[try
  [let file [open "data.txt"]?]
  [let data [read-all file]?]
  [Ok [parse data]?]]
```

### Panic

For unrecoverable errors (bugs, not expected failures):

```loon
[panic "invariant violated: x must be positive"]

; Or with unwrap (panics on Err/None):
[let val [some-option.unwrap]]
```

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

### Type-Aware Macros (see Section 14F)

Unlike traditional LISPs, Loon macros can optionally run *after* type inference, giving them access to type information. This enables powerful compile-time code generation.

```loon
[defmacro derive-serialize [T]
  [let fields [type-fields T]]
  `[impl Serialize ~T
     [fn serialize [&self writer : &mut Writer]
       ~@[map [fn [f]
                `[serialize-field writer ~(field-name f) self.~(field-name f)]]
              fields]]]]
```

---

## 10. Concurrency

### Async/Await

```loon
[async defn fetch-data [url : &str] : [Result String HttpError]
  [let response [await [http.get url]]?]
  [Ok response.body]]

[async defn main []
  [let data [await [fetch-data "https://api.example.com"]]?]
  [println data]]
```

### Channels

Message passing between concurrent tasks:

```loon
[let [tx rx] [channel]]

[spawn [fn []
  [send tx "hello from task"]]]

[let msg [await [recv rx]]]
[println msg]  ; "hello from task"
```

### No Shared Mutable State

The ownership model prevents data races at compile time. You can't share `&mut` across tasks. If you need shared state, use channels or explicit concurrent primitives:

```loon
[let counter [Atomic.new 0]]
[spawn [fn [] [counter.fetch-add 1]]]
```

---

## 11. WASM Target

### Compilation

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

Import functions from the WASM host:

```loon
[extern "env"
  [fn log [msg : &str]]]

[extern "wasi_snapshot_preview1"
  [fn fd-write [fd : i32 iovs : i32 iovs-len : i32 nwritten : i32] : i32]]
```

Export Loon functions to the host:

```loon
[pub extern defn handle-request [req : Request] : Response
  [Response.ok "hello from loon"]]
```

### Component Model

Loon supports the WASM Component Model for composing modules across languages:

```loon
[import "wasi:http/handler" {handle}]
[export "wasi:http/handler"
  [fn handle [req : IncomingRequest] : OutgoingResponse
    ...]]
```

---

## 12. Decentralized Package Manager ("Pond")

### Manifest: `loon.toml`

```toml
[package]
name = "my-app"
version = "0.1.0"
authors = ["alice <alice@example.com>"]

[dependencies]
http = { hash = "sha256:a1b2c3d4...", source = "ipfs" }
json = { hash = "sha256:e5f6g7h8...", source = "https://pond.loon.dev" }

[capabilities]
net = true
fs.read = ["./data/*"]
```

### Content-Addressed Identity

A package's identity is the hash of its source tree. Not a name, not a version — the hash.

```loon
[dep io.github.alice/http "sha256:a1b2c3d4e5f6..."]
```

- Names are vanity — convenient but not authoritative
- The hash is the truth — you always get exactly what you asked for
- No squatting, no typosquatting, no supply chain attacks via name confusion

### Distribution

Packages are distributed via content-addressed storage:

1. **IPFS** (default) — fully decentralized, anyone can pin
2. **HTTP** — any server can host packages (just serve the content at its hash)
3. **Git** — pin to a specific commit hash
4. **Pond Index** — optional discovery layer, maps names → hashes

### Commands

```sh
pond add io.github.alice/http     # resolve name → hash, add to loon.toml
pond audit                        # type-check all deps against their interfaces
pond verify                       # verify all dep hashes match their content
pond publish                      # hash your source, push to IPFS + index
pond search "http server"         # search the optional index
```

### Reproducible Builds

Since deps are hash-pinned and Loon compiles to WASM (a deterministic target), builds are reproducible by construction. Same source + same deps = same binary, always.

---

## 13. Example Programs

### Hello World

```loon
[defn main []
  [println "hello, world!"]]
```

### Fibonacci

```loon
[defn fib [n : u64] : u64
  [match n
    0 => 0
    1 => 1
    n => [+ [fib [- n 1]] [fib [- n 2]]]]]

[defn main []
  [|> [range 0 10]
    [map fib]
    [each [fn [x] [println x]]]]]
```

### HTTP Server

```loon
[use std.http {Server Request Response}]

[defn handle [req : &Request] : Response
  [match req.path
    "/"      => [Response.ok "welcome to loon!"]
    "/ping"  => [Response.ok "pong"]
    _        => [Response.not-found "404"]]]

[async defn main []
  [let server [Server.bind "0.0.0.0:8080"]]
  [println "listening on :8080"]
  [await [server.serve handle]]]
```

### CLI Tool: Word Counter

```loon
[use std.io {stdin read-to-string}]
[use std.collections {HashMap}]

[defn count-words [text : &str] : [HashMap String u64]
  [|> [split text " \n\t"]
    [filter [fn [w] [not [empty? w]]]]
    [fold [HashMap.new] [fn [acc word]
      [update acc word [fn [n] [+ [or n 0] 1]]]]]]]

[defn main []
  [let text [read-to-string stdin]?]
  [let counts [count-words &text]]
  [|> [entries counts]
    [sort-by [fn [[_ n]] n] :desc]
    [take 10]
    [each [fn [[word n]]
      [println [str word ": " n]]]]]]
```

---

## 14. What Makes Loon Legendary

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
loon> [fork]               ; branch this session — experiment freely
loon (fork-1)> [let y 999]
loon (fork-1)> [exit-fork]
loon> y
; error: unbound symbol 'y'  (fork was discarded)
```

**Key features:**
- **Persistent state.** Your REPL session is serializable. Close your laptop, reopen, pick up exactly where you left off.
- **Time travel.** `[rewind n]` steps back n evaluations. `[snapshot]` saves a named checkpoint. `[restore "checkpoint-name"]` jumps back.
- **Forking.** `[fork]` branches your session for experimentation. Keep or discard.
- **Hot reload.** Redefine functions, types, even trait implementations mid-session. The REPL re-typechecks incrementally.
- **The REPL is the debugger.** Set breakpoints with `[break-at module.fn]`. Inspect any value. Step through execution. All in the same session.

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

This matters for edge computing, embedded WASM, and anywhere binary size is a constraint.

### C. Pond — The Package Manager That Can't Be Rugged

(See Section 12 for full specification.)

The key insight: if packages are identified by their content hash, then:

- **No left-pad incident.** A package can't be "unpublished" — the hash still resolves from anyone who has it cached.
- **No supply chain attacks via name confusion.** The hash is the identity, not the name.
- **Built-in audit.** `pond audit` type-checks every dependency against its declared interface. If a dep's types don't match its claims, you know before you ship.
- **Gossip protocol for discovery.** No single point of failure. No npm Inc. Nodes share package metadata peer-to-peer.

```sh
$ pond audit
  ✓ http (sha256:a1b2c3d4) — 14 public fns, all type-safe
  ✓ json (sha256:e5f6g7h8) — 8 public fns, all type-safe
  ⚠ crypto (sha256:deadbeef) — uses unsafe in 2 locations
    → src/aes.loon:47  [unsafe block: raw memory access]
    → src/rng.loon:12  [unsafe block: FFI call]
  All deps verified.
```

### D. First-Class WASM Interop

Loon is Lua for the WASM era.

```loon
; Import a Rust-compiled WASM module as if it were Loon:
[use-wasm "image-resize.wasm" :as img]
[let resized [img.resize photo 800 600]]

; Export Loon functions for any WASM host:
[pub extern defn transform [input : &[u8]] : Vec<u8>
  [|> input [decode-json] [process] [encode-json]]]
```

**Interop features:**
- Import any WASM module with zero glue code — Loon reads the module's type signature
- Export Loon functions with Component Model types for seamless cross-language composition
- Works everywhere WASM runs: browsers, Wasmtime, Cloudflare Workers, Fastly Compute, Deno
- Bidirectional: Loon can be the host *or* the guest

### E. Error Messages That Teach

Every Loon error includes three things: **what** went wrong, **why** it's wrong, and **how** to fix it.

```
error[E0312]: cannot borrow `data` as mutable — already borrowed as immutable

   ┌─ src/main.loon:14:5
   │
12 │  [let ref [&data]]
   │            ───── immutable borrow occurs here
13 │  [println ref]
14 │  [push! data 42]
   │  ^^^^^^^^^^^^^^ mutable borrow attempted here
15 │  [println ref]
   │           ─── immutable borrow still in use here

   why: Loon prevents reading and writing the same data simultaneously
        to eliminate data races and dangling references.

   fix: Move the mutable operation before or after the immutable borrow:

   │  [push! data 42]     ; ← move this up
   │  [let ref [&data]]
   │  [println ref]
   │  [println ref]

   help: [explain E0312] for an interactive tutorial on borrowing
```

**Error features:**
- **Visual ownership diagrams** showing who owns what and where conflicts arise
- **`[explain E0312]`** opens an interactive REPL-based tutorial for that error class
- **Errors are data.** `[catch-errors [compile "src/main.loon"]]` returns a vector of structured error values — build your own error tooling

### F. Macros That See Types

In every other LISP, macros operate on syntax alone. They can't ask "what type is this expression?" Loon macros can.

Loon's macro system has two phases:
1. **Syntax macros** — traditional hygienic macros, expanded before type checking
2. **Type-aware macros** — expanded *after* type inference, with access to the type environment

```loon
[defmacro+ derive-debug [T]     ; the + means "type-aware"
  [let fields [type-fields T]]
  `[impl Debug ~T
     [fn debug [&self f : &mut Formatter] : FmtResult
       [write f ~(str (type-name T) " { ")]
       ~@[interpose
           [map [fn [field]
                  `[write f ~(str (field-name field) ": {:?} ") self.~(field-name field)]]
                fields]
           `[write f ", "]]
       [write f "}"]]]]
```

This enables:
- **Zero-cost generic serialization** — generate optimal serialize/deserialize at compile time
- **Derive macros** — `[#[derive Debug Serialize Eq]]` like Rust, but implemented in Loon itself
- **Compile-time ORMs** — generate type-safe queries from struct definitions
- Still hygienic — type-awareness doesn't mean type-unsafety

### G. Built-In Capability-Based Security

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

**Security properties:**
- A dependency can't phone home unless you grant `:net`
- A dependency can't read your filesystem unless you grant `:fs.read`
- Capabilities compose — if you don't grant `:net` to a dep, none of *its* deps get `:net` either
- Pairs perfectly with WASM's sandboxing model
- `pond audit` reports capability usage for every dependency

```sh
$ pond audit --capabilities
  http    — requires :net
  json    — requires nothing (pure)
  logger  — requires :fs.write ["./logs/*"]
  crypto  — requires nothing (pure)
```

### H. Notebooks as First-Class Programs

Loon supports two file types:
- `.loon` — regular source files
- `.loon.nb` — notebook files (literate programming)

Notebooks contain interleaved Markdown and code cells:

````loon-nb
# Data Analysis Pipeline

This notebook processes the Q4 sales data and generates a report.

```loon
[use std.csv {read-csv}]
[use std.stats {mean median std-dev}]

[let data [read-csv "./sales-q4.csv"]?]
[let revenue [map data [fn [row] row.revenue]]]
```

## Summary Statistics

```loon
[println [str "Mean revenue:   $" [mean revenue]]]
[println [str "Median revenue: $" [median revenue]]]
[println [str "Std deviation:  $" [std-dev revenue]]]
```
````

**Notebook features:**
- **Compile to WASM** — same as regular `.loon` files. No second-class citizens.
- **Render in browser** — via WASM, with live evaluation
- **Type-safe** — notebooks are type-checked like any other code
- **Ideal for:** science, data work, tutorials, documentation, and internal tools

### I. The Syntax Is the API

Because Loon is a LISP, the AST *is* the syntax. This makes tooling trivially derivable:

- **Formatter** — structurally format any Loon code (no ambiguity, no style debates)
- **Linter** — pattern-match on the AST directly
- **LSP** — autocomplete, go-to-definition, type-on-hover, all derived from the same data structure
- **Tree-sitter grammar** — near-trivial to write (it's just nested brackets)
- **AI-friendly** — LLMs can generate and manipulate Loon code structurally. It's just trees. No syntax edge cases, no operator precedence, no semicolon insertion.

```loon
; Code is data:
[let code [quote [+ 1 2]]]
[eval code]                    ; 3

; Transform code:
[let doubled [map code [fn [x]
  [if [number? x] [* x 2] x]]]]
[eval doubled]                 ; 6
```

`[quote]` and `[eval]` aren't just features — they're the extension model. Build your own DSLs, your own test frameworks, your own build tools — all in Loon.

### J. Emotional Design

Loon should feel good to use. The CLI, the error messages, the documentation — everything is designed with taste.

```
$ loon new my-project
  ✓ Created my-project/
  ✓ Created my-project/src/main.loon
  ✓ Created my-project/loon.toml
  → cd my-project && loon run

$ loon run
  Compiling my-project (847 bytes)
  hello, world!

$ loon test
  Running 3 tests...
  ✓ test-add ..................... 0.1ms
  ✓ test-greet .................. 0.2ms
  ✓ test-parse .................. 0.3ms
  All 3 tests passed (0.6ms)
```

**Design principles:**
- **Colors and Unicode** — box-drawing characters, check marks, spinners, syntax highlighting in terminal output
- **Bracket style in messages** — compiler examples use `[]` notation, dogfooding the syntax
- **Minimal scaffolding** — `loon new` creates exactly 2 files. No config sprawl.
- **Docs from notebooks** — the documentation site is generated from `.loon.nb` files, dogfooding the notebook system

---

## 15. Rust Safety Feature Parity

Every safety guarantee Rust provides, Loon provides:

| Rust Feature | Loon Equivalent |
|---|---|
| Ownership | Same model, more inference |
| Borrowing & lifetimes | Same model, lifetimes almost always elided |
| No null | `Option` / `Result` only |
| Pattern matching exhaustiveness | Enforced by compiler |
| `Send` / `Sync` | Inferred from ownership (values can only be sent if uniquely owned) |
| No data races | Ownership prevents shared mutable state |
| `unsafe` | `[unsafe ...]` blocks, same semantics — required for FFI and raw memory |
| Algebraic types | `[type ...]` with full pattern matching |
| Traits | `[trait ...]` / `[impl ...]` |
| `Result` / `?` operator | Same |
| `Drop` | `[trait Drop [fn drop [&mut self]]]` |
| Const generics | `[defn array [T n : usize] ...]` |

---

## 16. Syntax Comparison: Loon vs. Others

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

**Zig:**
```zig
fn fib(n: u64) u64 {
    return switch (n) {
        0 => 0,
        1 => 1,
        else => fib(n - 1) + fib(n - 2),
    };
}
```

### HTTP Handler — Loon vs. Rust

**Loon:**
```loon
[async defn handle [req : &Request] : Response
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

Loon is consistently more concise while retaining the same type safety.

---

## 17. Next Steps

1. **Finalize syntax choice** — gather feedback on Style A vs. Style B from this design doc
2. **Formal grammar** — write a PEG or EBNF grammar for the chosen syntax
3. **Rust implementation scaffold** — lexer, parser, AST types
4. **Type checker** — Hindley-Milner inference engine
5. **Ownership checker** — borrow checker inspired by Rust's but with more inference
6. **WASM codegen** — emit WASM bytecode
7. **REPL** — persistent, image-based
8. **Pond** — package manager MVP (local-only first, then IPFS)

---

*Loon: a LISP that flies.*
