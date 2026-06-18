# loon

<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="web/public/loon-light.png">
    <source media="(prefers-color-scheme: light)" srcset="web/public/loon.png">
    <img src="web/public/loon.png" alt="loon" width="200">
  </picture>
</p>

A functional language with invisible types, safe ownership, and algebraic effects.

**[Try it in your browser →](https://loonlang.com/play)**

```
[fn greet [name]
  [println "Hello, {name}!"]]

[pipe [range 1 10]
  [filter [fn [n] [> n 4]]]
  [map [fn [n] [* n n]]]
  [each println]]
```

## Features

- **Type Inference** — Full Hindley-Milner with let-polymorphism and row types. No annotations required.
- **Ownership** — Rust-style move semantics and borrow checking, inferred from dataflow. No lifetimes.
- **Algebraic Effects** — Declare, perform, and handle effects. Replaces exceptions, async, and mutable state.
- **Multi-shot continuations** — Handlers resume a captured continuation zero, one, or many times: nondeterministic search, backtracking, and durable replay from one program.
- **Handler towers** — One program runs unchanged under different handlers: deterministic offline tests, a recording replay, or real production IO. Swap the tower, not the code.
- **Async as effects** — Concurrency is ordinary effectful code with the scheduler as a handler: cooperative `spawn`/`yield`/`cancel` with structured concurrency. No function coloring.
- **Pattern Matching** — Positional pairs with destructuring and expression guards.
- **Type Methods** — Define methods inside `type` declarations with automatic dispatch.
- **Macros** — Template macros with quasiquoting. Procedural macros with compile-time IO.
- **Pipes** — Thread data through transformation chains. No nesting, no temp variables.
- **Language Server** — Diagnostics, hover types, go-to-definition, completions, inlay hints.
- **WASM** — Compiles to WebAssembly with closures, ADTs, and tree-shaking.

## Effect-native libraries

Two libraries share one effect/runtime substrate (`src/eff`, `src/http`, `src/agent`). The same program runs unchanged under prod / test / replay handler towers.

- **HTTP framework** (`src/http`) — routes are effectful functions whose inferred effect rows advertise their capabilities; a route can't touch the database without `Db` in its signature. Serves real HTTP over a TCP socket, or runs fully offline and deterministic under a test tower. Response bodies are moved and consumed on send, so a double-send is a compile error.
- **Agent framework** (`src/agent`) — the control loop is plain code performing `Llm`/`Tool`/`Approval`/`Memory` effects; the tower supplies the model, tools, approval policy, and memory. The same loop runs as a deterministic offline eval, a full trace, a human-in-the-loop approval round-trip, a multi-shot explorer over every approval outcome, durably (replay-then-resume), or served over HTTP.

## Quick Start

```bash
curl -fsSL https://loonlang.com/install.sh | sh
```

```bash
loon run hello.oo        # Run a program
loon repl                  # Interactive REPL with time-travel
loon fmt hello.oo        # Format source
loon explain E0201         # Interactive error tutorial
```

## Architecture

```
loon/
├── crates/
│   ├── loon-lang/        # Core: parser, type checker, interpreter
│   ├── loon-cli/         # CLI: run, repl, fmt, explain
│   ├── loon-lsp/         # Language server protocol
│   └── loon-wasm/        # WASM bindings for browser
├── web/                  # Website (written in Loon)
│   ├── public/           # Static assets, WASM bootstrap
│   └── src/              # Loon source: pages, components, router
├── tree-sitter-loon/     # Tree-sitter grammar
├── samples/              # Example programs
└── tests/                # Test suite
```

## Development

```bash
# Run tests
cargo test --workspace

# Build WASM + dev server
cd web && npm run dev

# Build language server
cargo build -p loon-lsp --release
```

## License

[MIT](LICENSE)
