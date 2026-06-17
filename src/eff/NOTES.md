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

## EFF-BUG-1 — generic ADT construction fails the type checker

Constructing any generic ADT trips `E0200`:

```
[type Option T [Some T] None]
[fn pick [b] [if b [Some "y"] None]]   ; error[E0200]: cannot unify T with String
```

Non-generic ADTs are fine (`samples/types.oo`'s `Shape` has concrete fields).
The declaration checks clean; only *construction* fails. Impact: the substrate
declares `Option`/`Result` (the shared contract) but the synchronous demo uses
plain `String`s instead of constructing them.

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

## EFF-LIMITATION-4 — host effects not wired into the EIR VM

On the default VM only `println` is a live host effect. `now`, `millis`, `uuid`,
`read-file`, `env`, and real `Net` are "value is not callable" / return `()`.
They exist for the legacy tree-walking interpreter / `net.rs`, not the EIR
backend. Impact: a *real* prod tower (real clock/random/socket/DB) cannot run on
the default VM yet — Stage 1's "prod" needs these wired into EIR. Deterministic
`test`/`replay` towers are fully unaffected.

## EFF-LIMITATION-5 — multi-file `use` doesn't run on the EIR VM

`[use math]` resolves at check time but is "value is not callable" at run time on
the EIR VM (the self-hosted frontend hit this too: "the whole frontend is one
file; later stages are concatenated, not imported"). Impact: Stage 0/1/2 share
definitions by living in one file / concatenation, not `use`, until fixed.

Also: the Rust prelude's `Option`/`Result` are not loaded on the EIR VM — define
shared types in-file.

## Async / scheduler status

A cooperative scheduler-as-a-handler (`Co.fork`/`Co.yield`, run-queue threaded as
the answer) **runs** on the VM (escaping continuations work) but: (a) does not
type-check (EFF-BUG-2), and (b) a multi-task run still drops later tasks (a
two-worker fork prints `P a`/`C a` but not the post-yield `P b`/`C b`). This is
a **separate** issue from EFF-BUG-3 (now fixed) — it involves re-capturing a
continuation that was itself created inside a resumed segment (EFF-BUG-6,
nested/re-entrant capture), and is the next thing to chase before async ships.
Async is therefore still deferred rather than shipped half-working.
