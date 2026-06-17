# Stage 0 substrate — status and foundation gaps

The synchronous effect substrate (`eff.oo`) is complete and **type-checks + runs
on the default EIR VM**: core effect interfaces, a representative program whose
inferred effect row is `#{Reader Clock Random Log Fail}`, three handler towers
(`test`, `prod`, `layered`) over the *same* program source, nested-handler
composition with effect propagation, and `Fail` as the sole error mechanism (no
exceptions anywhere).

Building it surfaced several **pre-existing** EIR-VM / type-checker issues
(confirmed present on `origin/main`, independent of the multi-shot change). They
gate a *clean* async substrate and Stage 1, so they are recorded here with
minimal repros. None are caused by this branch.

## What works today

- Multi-shot delimited continuations on the default VM (this branch). See
  `samples/multishot.oo`.
- Escaping continuations *run* correctly (e.g. a pure `State`, a counter).
- Tail-resumptive handlers, multi-clause handlers, and nested-handler
  composition with effect propagation **in isolation**.
- Effect-op signatures with applied/generic types: `[op [] [Option String]]`,
  `[op [[Vec Int]] a]`, polymorphic `State` — all parse and check.

## EFF-BUG-1 — generic ADT construction fails the type checker — FIXED

Root cause: `infer_type_def` collected type parameters by reading leading
symbols and **stopping at the first uppercase one** — but the prelude
convention writes parameters uppercase (`[type Option T [Some T] None]`). So
`T` was misread as a nullary constructor and `[Some 42]` produced
`E0200: cannot unify T with …`.

Fix (`crates/loon-lang/src/check/mod.rs`, `infer_type_def`): a leading bare
symbol is a type parameter iff it appears in some constructor's field positions
(e.g. `T` in `[Some T]`); otherwise it is a nullary constructor (`None`, or
`Red`/`Green`/`Blue`). Works for lowercase and uppercase parameters; enums and
mixed ADTs (`samples/types.oo`'s `Shape`) are unaffected. Regression test:
`generic_adt_construction`. `eff.oo` now uses `Env.lookup : String -> Option
String` with real `Some`/`None`.

## EFF-BUG-2 — escaping/answer-passing handlers don't type-check

`resume` is typed `a -> a` (one-shot, tail). The escaping style — a handler
clause that returns a function and uses `resume` non-tail — produces `E0203`
infinite type, even though it *runs*:

```
loon check samples/state.oo    ; error[E0203]: infinite type: t ~ t -> t
loon run   samples/state.oo    ; 11 / 55  (runs fine)
```

Impact: the cooperative scheduler and `State`-by-threading run but cannot
`loon check`. Proper effect-handler typing (answer type distinct from the op
result type) is the fix.

## EFF-BUG-3 — handler-stack teardown leak (correctness) — FIXED

Root cause: a **non-tail** resume re-establishes the handle's handlers on the
dynamic stack (`resume_continuation`, `base=Some`) so the continuation is
self-contained, but those re-established handlers were never removed. They
accumulated and a subsequent **nested** handle for the same effect found the
stale entry:

```
flat (handles R+L, non-tail resume)  then  nested (outer L, inner R)
  => R routed to the stale flat handler, not the live inner one  (BOTH/BOTH)
```

Fix (`crates/loon-lang/src/eir/vm.rs`): re-established handlers are marked
`ephemeral` and pruned by frame depth whenever frames shrink — on normal return
(`return_val`) and when a `perform` discards frames (`Op::Perform`). Lexical
`PushHandler`/`PopHandler` handlers are untouched. Regression test:
`vm_handler_isolation_across_handles`. `eff.oo`'s towers now run in any order.

## EFF-LIMITATION-4 — host effects not wired into the EIR VM — MOSTLY FIXED

The EIR VM's `builtin_effect` now implements the common host effects, so an
unhandled `IO.now` / `IO.millis` / `IO.uuid` / `IO.write-file` / `Process.env`
reaches a real implementation (wall clock, std-only v4 UUID, fs, env). A real
prod tower works on the default VM — see `src/eff/host_prod.oo`: the same
`make-id` runs under a deterministic `test` tower and a real-host `prod` tower
(`<uuid>@<unix-seconds>`). Regression test: `vm_host_effects`.

Still open: real `Net` (sockets/HTTP) is not yet wired into the EIR VM, and
`Process.env` returns `""` for an unset var rather than an `Option` (the VM
cannot construct a program-defined ADT tag from a builtin). Sockets are the
Stage-1 server's concern.

## EFF-LIMITATION-5 — multi-file `use` doesn't run on the EIR VM — FIXED

`loon run` now resolves `[use ...]` on the EIR VM. The lowering inlines each
imported module's macro-expanded definitions ahead of the program, registering
imported pub functions under their bare name (internal refs resolve) and a
qualified `alias.name` (so `[module.fn ...]` resolves). Qualified, selective
(`[use mod [f]]`), `as`-aliased, and transitive imports all work; cycles are
broken by a visited-set. Plumbing: `eval_eir_with_base_dir` threads the file's
directory; `lower::collect_imports` does the resolution. Regression test:
`vm_multi_file_use`.

Caveats: imported modules are linked into one flat namespace, so a name defined
in two modules collides (last wins) — distinct names, or qualified access, avoid
it. Stages can now `use` a shared `eff` module instead of concatenating.

Also: the Rust prelude's `Option`/`Result` are not loaded on the EIR VM — define
shared types in-file (or `use` a module that does).

## Async / scheduler status

A cooperative scheduler-as-a-handler (`Co.fork`/`Co.yield`, run-queue threaded as
the answer) **runs** on the VM (escaping continuations work) but: (a) does not
type-check (EFF-BUG-2), and (b) a multi-task run still drops later tasks (a
two-worker fork prints `P a`/`C a` but not the post-yield `P b`/`C b`). This is
a **separate** issue from EFF-BUG-3 (now fixed) — it involves re-capturing a
continuation that was itself created inside a resumed segment (EFF-BUG-6,
nested/re-entrant capture), and is the next thing to chase before async ships.
Async is therefore still deferred rather than shipped half-working.
