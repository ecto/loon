# Perf Log — Interpreter Optimization

## Baseline (pre-optimization)
- Bundle: 160KB, ~12,247 lines, 41 files
- Total boot: ~1037ms (Safari profile)
- Fetch: ~30ms | Eval: ~1008ms
- Breakdown unknown (no instrumentation)

## Phase 0: Instrumentation
- Added `[loon-perf]` console.log with parse/eval timing to `eval_ui_checked`
- No perf change, provides measurement for subsequent phases

## Phase 1: Skip type-check in production boot
- Removed `Checker::check_program()` from `eval_ui_checked` and `eval_ui`
- Type-checking still runs in playground (`eval_with_output`) and LSP (`check_program`)
- Est. savings: 50-150ms

## Phase 2: Remove per-expression sync_global_env
- Removed `sync_global_env(&env)` after every top-level expr (2 call sites)
- Now syncs once before calling `main()`
- Est. savings: 100-300ms (was O(N*M) with N exprs and M env size)

## Phase 3: Rc-shared global environment
- Global scope is now `Rc<RefCell<HashMap<String, Value>>>`, shared by reference
- `env.clone()` for closures only clones local scopes (typically empty/small)
- `merge_globals()` is a no-op when globals are shared (same Rc)
- `sync_global_env()` stores an Rc clone (pointer copy) instead of full HashMap clone
- Thread spawns use `deep_clone()` for safety
- Est. savings: 100-200ms

## Phase 4: String interning — DEFERRED
- Would require 160+ changes across parser, checker, macros, formatter, codegen
- Est. savings: 50-100ms — lowest ROI of all phases
- Can revisit if needed

## Phase 5: Rc-shared AST for function bodies
- Changed `LoonFn.clauses` body from `Vec<Expr>` to `Rc<[Expr]>`
- Cloning a function no longer deep-copies its AST body
- Est. savings: 30-50ms

## Results (Safari, measured)

| Metric | Before | After | Delta |
|--------|--------|-------|-------|
| Parse | unknown | 3ms | — |
| Eval | ~1008ms | 359ms | **-649ms (64%)** |
| Total boot | ~1037ms | 381ms | **-656ms (63%)** |

## Summary
- Phases completed: 0, 1, 2, 3, 5
- Actual savings: ~650ms (eval 1008ms → 359ms)
- All 249 tests passing (160 unit + 4 module + 63 interp + 22 macro)
- WASM + LSP build clean
