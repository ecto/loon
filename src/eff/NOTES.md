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

## EFF-BUG-3 — handler-stack teardown leak (correctness)

A flat handler that handles effect `E` leaks its handler entry when `E` is
performed: the VM jumps into the handler at the perform site and pops the prompt
frame, so the `PopHandler`s emitted after the body never run
(`crates/loon-lang/src/eir/vm.rs`, `Op::Perform` vs `Op::PushHandler`/
`PopHandler`). A subsequent **nested** handle for the same effect then finds the
stale handler:

```
both (handles R+L, flat)  then  nested (outer L, inner R)
  => R routes to the stale `both` handler, not the live inner one
```

Minimal repro (`both` then `nested` prints `BOTH BOTH`, should be `BOTH INNER`):
see the probe in the Stage-0 investigation. This directly undermines the
composition story for sequential towers (and would corrupt per-request towers in
Stage 1). `eff.oo`'s demo orders the nested tower first to stay correct; the bug
must be fixed before Stage 1.

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
type-check (EFF-BUG-2), and (b) a multi-task run currently drops later tasks —
likely the same teardown issue as EFF-BUG-3 surfacing under nested re-entrant
resumption. Async is therefore deferred behind these fixes rather than shipped
half-working.
