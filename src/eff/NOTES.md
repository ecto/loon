# Stage 0 substrate — status and foundation gaps

## Agent framework — control loop as effects, tower decides everything

`src/agent/agent.oo`: the agent control loop is plain code performing
`Llm`/`Tool`/`Approval`/`Memory`/`Log` effects; the handler tower supplies the
model, tools, approval policy, and memory. The SAME loop runs under four towers
unchanged — `under-test` (scripted model + mocked tools, deterministic and
offline), `under-trace` (records every interaction), `under-deny` (a
human-in-the-loop approval that denies one action — the `Approval.request`
suspend/resume round trip), and `explore` (multi-shot: resume each approval BOTH
ways to visit every approve/deny world — backtracking from one program).
Regression test: `vm_agent_under_towers`.

The prod tower (real LLM API + MCP tools) needs an outbound HTTP client wired
into the VM (the server side — `Net.listen`/`accept`/`send` — is done; the
client `Net.get`/`post` is behind the `pkg-fetch`/ureq feature and not yet wired
into the EIR builtin effects). The offline test/trace/deny/explore towers — the
ship-gate items (deterministic multi-step replay; approval round trip) — work
today with no network.

A checker fix this needed: `resolve_type_name` was missing the `Keyword` case
(only `name_to_type` had it), so a user-effect op declared to return `Keyword`
got a fresh type var, which defeated the ownership checker's Copy detection and
produced a false use-after-move when the result was used in two effects. Added
the arm; effect-result keywords are now Copy as expected.

## Async substrate (Stage 0) — cooperative scheduler as a handler

`src/eff/async.oo` completes Stage 0's async story: concurrency is ordinary code
performing effects (`Co.fork`/`yield`/`exit`/`cancel`), with the SCHEDULER as a
handler — no async "color" on functions, no runtime async primitive. It
type-checks AND runs on the default EIR VM (built on the multi-shot delimited
continuations proven in `src/eff/scheduler.oo`).

- `spawn` (built on `fork`) runs a thunk as a background child; tasks interleave
  cooperatively at `yield` (round-robin: `A1 B1 A2 B2 A3 B3`).
- Structured concurrency is designed in: the scheduler IS the scope and drains
  all children before returning; `cancel` cancels the scope — every pending
  child is dropped (no orphans outlive the scope). Regression test:
  `vm_cooperative_scheduler`.

Remaining async op — `await` (a backgrounded task's *result*): returning a value
from a spawned task needs result-passing (a Future/result-slot). Loon has no
mutable cells on the EIR VM and state is itself an effect, so this wants either a
state-threaded scheduler variant or a `Future` effect — the documented next
async iteration. Awaiting a child *computation* inline is just running it (it
participates in cooperative scheduling via `yield`).

## Backend unification (in progress)

A differential-parity suite (`crates/loon-lang/tests/backend_parity.rs`) runs the
same programs under the EIR VM and the legacy interpreter and asserts identical
output — the safety net for collapsing the backends onto one IR. It immediately
caught a real bug: the EIR VM's `map`/`filter`/`fold` only handled the
pipe/thread-last argument order (`[map fn coll]`), so the *direct* form
(`[map coll fn]`) silently returned `()`/the input. Fixed by detecting the
collection vs the function by TYPE (mirroring the interpreter), so both forms
work. Two divergences remain PINNED as tests (a worklist, not silent drift):
the legacy interp's non-resuming handler clause wrongly resumes (EIR's abort is
correct), and a binary builtin passed as a HOF arg (`[fold xs 0 +]`) misfires on
the EIR VM (arity-1 builtin-as-value wrapper). See the suite for details.



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

## EFF-BUG-2 — escaping/answer-passing handlers don't type-check — FIXED

`resume` was typed `a -> a`, so the escaping style — a clause that returns a
function and uses `resume` non-tail — was an infinite type (`E0203`).

Fix, in two parts (`check/mod.rs`, `check/ownership.rs`):
1. `infer_handle` now gives the handler a single **answer** type: `resume :
   op-result -> answer` (argument and result independent), every clause body and
   the `return` clause unify with the answer, and the `handle` evaluates to the
   answer. With no `return` clause the answer is just the body's type, so
   tail-resumptive handlers are unchanged.
2. The ownership checker treats `resume` as borrowing (not moving) its argument
   — a multi-shot continuation may be resumed repeatedly, so resume must not
   consume the value. This removes a false `E0300` move on `[[resume s] s]`.

`loon check samples/state.oo` is now clean and it still runs (11 / 55).
Regression test: `escaping_handler_type_checks`. The cooperative scheduler's
handlers now type-check too (its remaining runtime gap is EFF-BUG-6, below).

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

Real `Net` sockets are now wired too (`crates/loon-lang/src/eir/net.rs`):
`Net.listen` / `Net.accept` (-> `[method path body]`) / `Net.send` give a
blocking one-request-at-a-time HTTP server that fits the single-threaded VM. A
Loon program serves real HTTP over TCP — see `src/http/serve.oo`; round-trip
test: `real_http_round_trip`.

Still open: `Process.env` returns `""` for an unset var rather than an `Option`
(the VM cannot construct a program-defined ADT tag from a builtin); and the
ownership checker does not yet treat a Copy-typed *parameter* as Copy across a
consuming call (a false use-after-move — work around by reusing a let-bound Int
or inlining the literal; see `src/http/serve.oo`).

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

## EFF-BUG-6 — "re-entrant continuation capture" — NOT A BUG (test-harness error)

**Correction.** Earlier notes claimed interleaved scheduling was broken on the
EIR VM and needed a continuation-engine rework. That was wrong — the defect was
in the *scheduler test code*, not the VM.

The minimal scheduler's `run-next` did `[drop q 1]`, but `drop` is **count-first**
(`[drop n coll]`), so `[drop q 1]` passed the queue as the count and `1` as the
collection and returned `()`. The next task was therefore invoked with `()`
instead of the remaining queue, so its yielded continuation was never enqueued —
which *looked* like a lost continuation. Writing it `[drop 1 q]` fixes it.

With that corrected, the cooperative scheduler as an effect handler RUNS
correctly on the default EIR VM, including INTERLEAVED, re-entrant multi-shot
resumption — two workers yielding twice round-robin produce
`A1 B1 A2 B2 A3 B3`. See `src/eff/scheduler.oo`. The EIR VM's delimited
continuations are sound for this; no engine rework is needed, and async is NOT
blocked.

### Type-checking the recursive answer type — RESOLVED via a nominal wrapper

The scheduler's answer type is recursive — the queue is a `Vec` of resumptions,
each a function *of the queue* (`Queue = Vec (Queue -> Unit)`), i.e.
`t ~ Vec t -> u`, which Hindley-Milner rejects as infinite (E0203). Boxing the
resumption in a nominal type — `[type Cont [MkCont [-> [Vec Cont] Unit]]]` —
makes the queue type finite (`Vec Cont`), since HM treats the named `Cont`
nominally and does not expand it. `run-next` then `match`es `[MkCont f]` to
unwrap. With that, `src/eff/scheduler.oo` BOTH `loon check`s clean AND runs.

Net: the cooperative scheduler-as-a-handler — the async substrate — is complete
on the default EIR VM (type-checks + runs, interleaved multi-shot). No engine
rework and no bytecode VM were needed for it.
