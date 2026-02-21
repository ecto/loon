# Loon

A functional language with invisible types, safe ownership, and algebraic effects.

## File Extensions

- `.oo` is the primary source file extension (from l-**oo**-n)
- `.loon` is supported as a backward-compatible fallback
- Manifest: `pkg.oo` (falls back to `pkg.loon`)
- Lockfile: `lock.oo` (falls back to `lock.loon`)

## Project Structure

```
loon/
├── crates/
│   ├── loon-lang/        # Core: parser, type checker, interpreter
│   ├── loon-cli/         # CLI: run, repl, fmt, explain
│   ├── loon-lsp/         # Language server protocol
│   └── loon-wasm/        # WASM bindings for browser
├── web/                  # Website (written in Loon, uses .loon files)
│   ├── public/           # Static assets, WASM bootstrap
│   └── src/              # Loon source: pages, components, router
├── tree-sitter-loon/     # Tree-sitter grammar
├── samples/              # Example programs (.oo files)
└── docs/                 # Documentation
```

## Versioning

- Version is derived from git tags at build time (`build.rs` in loon-cli)
- `loon --version` shows the git-described version (e.g. `0.4.20`, `0.4.20-3-gabcdef0`)
- To release: `git tag v0.X.Y && git push --tags`
- `Cargo.toml` workspace version is a fallback only
- The web changelog at `web/src/pages/blog.loon` is the user-facing changelog

## Development

```bash
cargo test --workspace          # Run all tests
cargo run -p loon-cli -- run samples/hello.oo   # Run a sample
cargo run -p loon-cli -- fmt samples/           # Format files
cargo run -p loon-cli -- new test-proj          # Creates pkg.oo + src/main.oo
```

## Key Patterns

- Module resolution: tries `.oo` first, falls back to `.loon`
- Manifest/lockfile loading: tries `.oo` first, falls back to `.loon`
- New file creation always uses `.oo`
- `collect_loon_files` in CLI accepts both `.oo` and `.loon` extensions
- web/src/ files remain `.loon` (separate concern, large rename)
