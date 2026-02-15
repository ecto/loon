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
- **Natural language mode.** Type brackets → it's code. Type English → it's a prompt. The REPL detects which one you meant and seamlessly switches.

#### The REPL Speaks English

The detection is trivial: input that starts with `[` or is a bare symbol is code. Everything else is natural language. No mode-switching command, no prefix character — just type.

```loon
loon> [+ 1 2]
3

loon> what does the greet function do?
  greet takes a String and returns "hello, {name}!"
  Defined at src/main.loon:3. Last modified 2 minutes ago.

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
    load-config  (src/config.loon:12) — constructs Config
    validate     (src/config.loon:28) — borrows &Config
    main         (src/main.loon:5)    — owns Config
```

**The AI has full context.** It can see every binding in your REPL session, every function in your project, every type definition. It doesn't guess — it reads the AST, the type environment, and the dependency graph. When it proposes code, that code is type-checked before you see it.

**It composes with everything else:**
- Ask "why did that error?" and it explains the last error using the same teaching format as the compiler (Section 14E)
- Ask "what changed since my last snapshot?" and it diffs against your REPL history (time travel)
- Ask "make this function pure" and it refactors effects out, verified by the effect system (Section 14L)
- Ask "what capabilities does this dep use?" and it queries the capability system (Section 14G)

**The REPL is Claude Code for Loon.** But better — because the AI operates on typed ASTs, not raw text. It can't propose code that doesn't type-check. It can't propose code that violates ownership. It can't propose code that exceeds granted capabilities. The language *constrains* the AI's output to be correct.

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

### K. Content-Addressed Definitions

Stolen shamelessly from Unison — and extended.

Every function and type in Loon is identified not by its name but by the hash of its *content* (its AST after desugaring). Names are metadata, not identity.

```loon
loon> [hash greet]
sha256:7f3a...  ; the identity of this function

loon> [rename greet say-hello]
; same hash — renaming doesn't change identity

loon> [history greet]
  sha256:7f3a... (current)  — added "!" to greeting
  sha256:2b1c... (2 edits ago) — initial implementation
  sha256:9e0d... (5 edits ago) — prototype
```

**Consequences:**
- **Merge conflicts on renames are impossible.** Two people rename the same function differently? Both names point to the same hash. No conflict.
- **Refactoring is free.** Move a function to a different module? The hash doesn't change. Nothing breaks.
- **Codebase is a persistent database.** Every version of every definition is stored. `[history fn-name]` shows the full evolution. `[diff sha256:7f3a sha256:2b1c]` shows what changed.
- **Dead code is provable.** If no live hash references your definition's hash, it's dead. Not heuristically — mathematically.
- **Pairs with Pond.** Packages are content-addressed. Definitions are content-addressed. It's hashes all the way down.

### L. Algebraic Effects

Async/await is just one effect. Exceptions are just one effect. Loon generalizes all of them.

An algebraic effect is a declared side effect that a function *performs* and a caller *handles*. Think of it as resumable exceptions with a type system.

```loon
; Declare effects:
[effect IO
  [fn read-file [path : &str] : String]
  [fn write-file [path : &str content : &str]]]

[effect Fail
  [fn fail [msg : String] : !]]   ; ! means "never returns normally"

; Use effects in function signatures:
[defn load-config [path : &str] : Config / {IO Fail}
  [let raw [IO.read-file path]]
  [if [empty? raw]
    [Fail.fail "config file is empty"]]
  [parse-toml raw]]

; Handle effects at the call site:
[handle [load-config "app.toml"]
  [IO.read-file path] => [resume [mock-fs.read path]]   ; inject mock FS
  [IO.write-file _ _] => [resume]                         ; swallow writes
  [Fail.fail msg]     => [Config.default]]                ; recover gracefully
```

**Why this is legendary:**
- **Testing without mocks.** Handle the `IO` effect with test data — no dependency injection frameworks, no mock libraries, no test-only interfaces.
- **Composable.** Effects compose naturally. A function with `{IO Fail Log}` effects can be partially handled — handle `Log`, pass `IO` and `Fail` through.
- **Async is just an effect.** `[effect Async [fn await [future : Future T] : T]]` — the runtime provides the handler. You can provide a different handler for testing (instant resolution) or simulation (deterministic scheduling).
- **Replaces 5 language features with 1.** Exceptions, async/await, generators, dependency injection, and algebraic state — all expressible as effects.
- **Informed by research.** Draws from Koka, Eff, and OCaml 5. No mainstream language has shipped this yet.

### M. Incremental Computation

Loon's runtime includes a built-in incremental computation engine, inspired by Salsa (the engine inside rust-analyzer).

```loon
[memo defn parse [source : &str] : Ast
  [parser.parse source]]

[memo defn typecheck [ast : &Ast] : TypedAst
  [checker.check ast]]

[memo defn codegen [typed : &TypedAst] : Wasm
  [emitter.emit typed]]

[memo defn compile [source : &str] : Wasm
  [|> source [parse] [typecheck] [codegen]]]
```

When `source` changes, only the affected stages re-run. The runtime automatically tracks which `[memo]` functions depend on which inputs and invalidates the minimal set.

**Applications:**
- **The Loon compiler itself** uses incremental computation — edit one function, only that function gets re-typechecked and re-compiled. Instant feedback.
- **Reactive UIs.** `[memo]` functions + a render loop = a reactive framework with zero library code.
- **Data pipelines.** Change one input CSV, only the downstream transformations re-run.
- **Build systems.** `[memo]` is `make` but type-safe and automatic.

### N. Structural Editing

Since Loon is a LISP, syntax errors are a *choice* — and Loon chooses to eliminate them.

The blessed editing experience is structural: the cursor moves between AST nodes, not characters. Insertion, deletion, and transformation operate on trees, not text.

```
; Cursor is on the [+ x 1] node:
[defn inc [x] «[+ x 1]»]

; Press "w" to wrap in a new form:
[defn inc [x] [«▮» [+ x 1]]]

; Type "if [> x 0]":
[defn inc [x] [if [> x 0] [+ x 1] «▮»]]

; Type "0" to complete:
[defn inc [x] [if [> x 0] [+ x 1] 0]]
```

**Shipped as:**
- **VS Code extension** — structural navigation and editing keybindings
- **Web editor** — runs in the browser via WASM (dogfooding the WASM target)
- **REPL integration** — structural editing in the terminal REPL
- **Fallback** — you can always edit Loon as plain text. Structural editing is the blessed path, not a cage.

**Why this matters:**
- Syntax errors become impossible in structural mode
- The formatter isn't a tool — it's just how the code renders
- Refactoring is node manipulation, not text search-and-replace
- Pairs perfectly with the content-addressed definitions (K) — edits produce new hashes automatically

### O. Provenance Tracking

Every value in Loon can carry invisible metadata about its origin — where it was created, how it was transformed, where it traveled.

```loon
[let name [read-input "What's your name?"]]
[let greeting [str "hello, " name]]
[let upper [to-upper greeting]]

[provenance upper]
; => #[
;   {origin: "stdin" line: 1 fn: read-input}
;   {transform: str line: 2 fn: main}
;   {transform: to-upper line: 3 fn: main}
; ]
```

**Zero-cost when unobserved.** Provenance metadata is tracked lazily — the compiler elides it entirely unless someone calls `[provenance]` or opts in with `[#[track] let ...]`. No runtime cost for code that doesn't use it.

**Applications:**
- **Debugging.** "Where did this value come from?" is answered instantly — no printf archaeology.
- **Security auditing.** Track tainted data from user input through your entire program. `[tainted? val]` returns true if the value originated from an untrusted source.
- **Data pipelines.** Every output cell in a notebook knows exactly which input cells contributed to it.
- **Compliance.** "Show me every value derived from PII" becomes a one-liner.

### P. Transparent Persistence

Loon's persistent data structures aren't just immutable in memory — they can be transparently backed by durable storage.

```loon
; Create a persistent store (backed by disk):
[let db [Store.open "./app.db"]]

; Use it like a normal map:
[let db [assoc db :users #[
  {:name "alice" :role :admin}
  {:name "bob" :role :user}]]]

; It's durable — survives process restarts:
[let db [Store.open "./app.db"]]
[get db :users]  ; => still there

; Query it with normal Loon functions:
[|> [get db :users]
  [filter [fn [u] [= u.role :admin]]]
  [map :name]]
; => #["alice"]
```

**Key properties:**
- **No ORM.** Your data structures *are* your database. Same functions, same types, same everything.
- **No serialization.** The persistent data structure format *is* the on-disk format. No encoding/decoding step.
- **Immutable history.** Since the backing store is persistent (append-only), you get time-travel for free: `[Store.at db timestamp]` returns the database as it was at that point.
- **Transactional.** Multiple mutations in a `[transact db ...]` block are atomic.
- **Scales down.** For small projects, it's a JSON file with superpowers. For large projects, it's a proper embedded database. Same API either way.

### Q. First-Class LLM & Agentic Programming

This is the big one. Loon is designed from the ground up to be the best language for writing AI-powered programs — and the best language for AI to write programs *in*.

#### The Language AI Can Actually Write

Loon's LISP syntax isn't just elegant — it's *structurally trivial* for LLMs. There's no operator precedence, no semicolon insertion, no whitespace ambiguity, no brace-matching across 50 lines. Code is trees. LLMs are good at trees.

```loon
; An LLM generating Loon doesn't need to track:
;   - indentation rules (Python)
;   - semicolons and braces (C, Java, Rust)
;   - operator precedence (everything)
;   - macro hygiene (it's automatic)
; It just needs to produce balanced brackets with valid symbols.
```

But Loon goes further than just being easy to generate. It has *language-level primitives* for AI-assisted programming.

#### `[ai ...]` — Compile-Time Code Generation

```loon
; Generate a function at compile time using an LLM:
[ai defn celsius-to-fahrenheit [c : f64] : f64
  "Convert Celsius to Fahrenheit"]
; The compiler sends the signature + docstring to an LLM,
; receives an implementation, type-checks it, and compiles it.
; If it doesn't type-check, the compiler retries (up to 3x) then errors.

; Generate a type from a description:
[ai type HttpStatus
  "Standard HTTP status codes as an enum with variants like Ok, NotFound, etc."]

; Generate a trait implementation:
[ai impl Display HttpStatus
  "Format status codes as 'CODE MESSAGE', e.g. '200 OK'"]
```

**Crucially:** `[ai ...]` blocks are **verified by the compiler**. The LLM proposes, the type system disposes. Generated code must pass type checking, ownership checking, and capability checking before it's accepted. The LLM is a code generator, not a trusted authority.

```sh
$ loon build
  [ai] Generating celsius-to-fahrenheit... ✓ (type-checked, 1 attempt)
  [ai] Generating HttpStatus... ✓ (type-checked, 2 attempts — first had duplicate variant)
  Compiled main.wasm (1.2KB)
```

Generated code is cached by its prompt hash + model version. Rebuilds don't re-query the LLM unless the prompt changes. `loon build --offline` refuses to call any LLM and uses only cached generations.

#### `[agent ...]` — Agent Loops as a Language Primitive

```loon
[defn research-agent [question : String] : String / {AI Net}
  [agent
    :system "You are a research assistant. Answer questions using web search."
    :tools [web-search summarize]       ; Loon functions exposed as tools
    :max-turns 10
    :prompt question]]

; The tools are just normal Loon functions with type signatures:
[#[tool "Search the web for a query"]
 defn web-search [query : String] : [Vec SearchResult] / {Net}
  [http.get [str "https://api.search.io?q=" [url-encode query]]]]

[#[tool "Summarize a long text"]
 defn summarize [text : String] : String / {AI}
  [ai.complete [str "Summarize this:\n" text]]]
```

**The `#[tool]` attribute** auto-generates a JSON schema from the function's type signature. No manual schema writing. No SDK. The type system *is* the tool specification.

**Agent features:**
- **Tool type safety.** If a tool returns `Result`, the agent runtime handles errors automatically. If a tool requires `Net` capability, the agent must have that capability granted.
- **Observability.** Every agent turn is logged as structured data. `[agent-trace result]` returns the full chain of reasoning, tool calls, and responses.
- **Deterministic replay.** Agent traces are serializable. `[replay trace]` re-executes an agent run with the same tool responses but a different system prompt — for testing, debugging, and evaluation.
- **Composable.** Agents can call other agents. An orchestrator agent can delegate to specialist agents, each with different tools and capabilities.

#### Structured Output — Types as Schemas

```loon
[type MovieReview
  [title : String]
  [rating : f64]
  [pros : [Vec String]]
  [cons : [Vec String]]
  [summary : String]]

; Ask an LLM to produce a typed value:
[let review : MovieReview
  [ai.extract "Review the movie Dune Part 2"]]

; The LLM's output is parsed and validated against the Loon type.
; Fields are type-checked. Missing fields are caught. Extra fields are dropped.
review.rating  ; 8.5 — it's a real f64, not a string
```

This works because Loon types map cleanly to JSON Schema (algebraic types → discriminated unions, Option → nullable, etc.). The compiler generates schemas from types automatically.

#### Semantic Functions — LLM Calls That Look Like Functions

```loon
[semantic defn classify-sentiment [text : String] : Sentiment
  "Classify the sentiment of the given text."]

[semantic defn translate [text : String lang : Language] : String
  "Translate the text to the target language. Preserve tone and idiom."]

[semantic defn extract-entities [text : String] : [Vec Entity]
  "Extract named entities (people, places, organizations) from the text."]

; Use them like any other function:
[|> [read-file "feedback.txt"]?
  [lines]
  [map [fn [line] {
    :text line
    :sentiment [classify-sentiment line]
    :entities [extract-entities line]}]]
  [filter [fn [r] [= r.sentiment Sentiment.Negative]]]
  [each [fn [r] [println [str "Negative: " r.text]]]]]
```

`[semantic defn ...]` declares a function whose implementation *is* a natural language prompt. The return type constrains the LLM's output. The function is called like any other — you can `map` it, `filter` with it, compose it. But under the hood, it's an LLM call with structured output extraction.

**Key design decisions:**
- Semantic functions are **marked in the type system** with an `AI` effect — you always know what's hitting an LLM and what's pure computation.
- They're **cacheable** — same input → same output (configurable, opt-out for non-deterministic use cases).
- They **compose with the pipe operator** and all other Loon features. No special syntax for "AI code" vs "normal code."
- They **respect capabilities** — a semantic function that needs `Net` to call an API must declare it.

#### Sandboxed Code Execution

The killer combination: LLMs generate Loon code + the capability system sandboxes it + WASM isolates it.

```loon
; Let an LLM write and execute code, safely:
[let code [ai.complete "Write a Loon function that sorts a list of numbers"]]
[let ast [parse code]?]                        ; parse to AST
[let typed [typecheck ast]?]                   ; must type-check
[let checked [capability-check ast #{}]]       ; grant NO capabilities
[let result [eval-sandboxed checked #[3 1 4 1 5]]]  ; run in WASM sandbox
; => #[1 1 3 4 5]
```

An LLM can propose code. The code is parsed, type-checked, capability-checked (you control exactly what it can do), and executed in a WASM sandbox. If the LLM generates code that tries to read the filesystem, it fails at capability checking — before it ever runs.

**This is Loon's superpower for agentic AI:** the language itself provides the guardrails. No separate sandbox runtime. No container overhead. The type system and capability system *are* the sandbox.

#### Model Configuration

```toml
# loon.toml
[ai]
default-model = "claude-sonnet"
compile-model = "claude-haiku"     # fast model for [ai defn] blocks
agent-model = "claude-opus"        # capable model for [agent] blocks
cache = true                        # cache LLM responses by prompt hash
offline = false                     # if true, only use cached responses
```

```loon
; Override per-call:
[ai.with-model "claude-opus"
  [ai defn complex-algorithm [data : &[f64]] : [Vec f64]
    "Implement a fast Fourier transform"]]
```

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
