# Self-hosting Stage 3 — the compiler (plan)

Status: **planning** (no compiler code yet). Stages 0–2 are merged on `main`:
reader (`src/frontend/reader.oo`), comment/blank attachment + formatter
(`comments.oo`, `formatter.oo`), and the macro expander (`expander.oo`). This
doc plans Stage 3 — type inference, effects, ownership, EIR lowering, and a
self-hosted EIR VM — and records the decisions that scope it.

Read `ARCHITECTURE.md` first for the survey of the Rust implementation and the
D1–D3 decisions; this doc builds on them.

## What the Rust compiler does (target to reproduce)

Pipeline: `parse → macro-expand → typecheck → lower to EIR → backend`.
`loon run` uses the **EIR register VM** by default (WASM/native are secondary).

- **Types** (`types/mod.rs`, `check/mod.rs` ~5165 lines): Hindley-Milner.
  Robinson `unify` with occurs check, `generalize`/`instantiate`, `Scheme`s,
  trait bounds, and structural **record rows**. "Invisible types" = full
  inference; `[sig …]` is optional.
- **Effects** (`effects/mod.rs`): a **flat `EffectSet` (set of names)** — *not*
  row-based. `resume : a → a`. Our **D1** chooses row-based effects, so the
  self-hosted checker deliberately diverges here.
- **Ownership** (`check/ownership.rs` ~925 lines): dataflow move/borrow/copy
  (`Alive`/`Moved`/`MutBorrowed`, param modes Borrow/MutBorrow/Move).
- **EIR** (`eir/*`, ~8459 lines): flat SSA-ish IR (~17 ops, blocks +
  terminators), **evidence-passing effects**, **one-shot** continuations
  (`resume` is an identity closure). Our **D3** chooses multi-shot.

## Decisions (this stage)

- **Fixpoint = F1 + F2** (not F3):
  - **F1 behavioral** — the self-hosted toolchain processes the corpus
    identically to `loon run`.
  - **F2 self-run** — the self-hosted toolchain runs on *its own source*.
  - **Not F3** (byte-identical EIR/WASM to the Rust compiler): incompatible
    with D1 (row effects) and D3 (multi-shot); explicitly out of scope.
- **Backend = EIR + self-hosted EIR VM.** Reproduce the IR shape and lowering,
  and write the register VM in Loon. WASM/native backends are out of scope.
- **Ownership = in v1.**
- **D1 row effects / D3 multi-shot** carried from `ARCHITECTURE.md`.

### How D3 (multi-shot) reconciles with the one-shot host VM

The *compiler itself never needs multi-shot to run*, so it is written in a
one-shot-safe subset → it runs on today's Rust VM and on its own EIR VM, so the
**F2 fixpoint does not depend on multi-shot**. Multi-shot is a *target-language*
capability: we add **reified, resumable continuations to the self-hosted EIR
VM** (segmented/copied frames) — the "new runtime support" `ARCHITECTURE.md`
flagged — so `resume` can be called more than once. This is something the Rust
VM lacks, so self-hosting *adds* a capability rather than only mirroring.
Multi-shot is validated by conformance tests, independent of the fixpoint.

## Sub-stages (each its own PR, each gated)

Every sub-stage is concatenated after the existing frontend (the EIR VM has no
working `use`), tested in isolation, then against the corpus. Differential
testing uses the Rust toolchain as oracle: `loon check` (accept/reject + error
text) for the checker, `loon run` (stdout) for behavior.

### 3a — Types + HM core  → `src/frontend/types.oo`
`Type` ADT (`TVar TFn TCon TTuple TRow TRecord` + base types + effect rows),
a `Subst` (fresh-var counter + `var→Type` map, threaded functionally like the
attachment pass), `resolve`, `occurs`, `unify`, `bind`, `generalize`,
`instantiate`, `Scheme`, `TypeEnv`.
**Gate:** unit tests (unify success/clash/occurs, generalize/instantiate
freshening) + differential vs `loon check` accept/reject on crafted snippets.

### 3b — Inference over Forms  → `src/frontend/infer.oo`
`infer(env, subst, form) → (type, subst)` threaded. Literals, `FSym`
lookup+instantiate, lambda/`fn`, `let`, `if` (unify branches), `match` (pattern
types + exhaustiveness later), application (unify fn vs args), collections
(`Vec`/`Map`/`Set`/`Tuple`), record literals → rows, and `[type …]` decls
(register constructors).
**Gate:** differential vs `loon check` accept/reject over the corpus; principal
-type spot checks on snippets.

### 3c — Effect rows (D1)  → folded into `infer.oo` / `effects.oo`
Effect-row type (effect-name fields + optional tail var), `unify` over effect
rows, function types carrying an effect row, `perform`/`handle` (handle
subtracts handled effects, leaving the tail open). Register `[effect …]` ops.
**Gate:** the homepage `compose` example type-checks; a Loon-only effect
conformance suite (no Rust differential — Rust is flat-set, per D1).

### 3d — Ownership / borrow  → `src/frontend/ownership.oo`
Dataflow over Forms: `BindingState` (Alive/Moved/MutBorrowed), `ParamMode`
inference, use-after-move, conflicting borrows, copy types (`[derive Copy]`).
**Gate:** differential vs `loon check` ownership diagnostics on the corpus +
crafted move/borrow cases.

### 3e — EIR + lowering  → `src/frontend/eir.oo`, `src/frontend/lower.oo`
EIR data structures (`Module/Func/Block/Op/End`, `Reg/FuncId/BlockId/StringId`).
Lower typed Forms → EIR: register allocation (threaded `next-reg`), blocks,
closures (`Close` + captures), `Call`/`Invoke`, `if`→`Br`, `match`→`Tag`+
`Switch`, `let`, collection builders, effects (`Perform`/`PushHandler`/
`PopHandler`), tail calls (`Tail`/`Recur`).
**Gate:** EIR well-formedness checks; end-to-end correctness once 3f runs it.

### 3f — Self-hosted EIR VM  → `src/frontend/vm.oo`
Register VM in Loon: frames, block dispatch, op execution, heap objects
(`Vec/Map/Set/Tuple/Adt/Closure/Str`), builtins, evidence + dynamic handler
stack. **Multi-shot (D3) extension:** reified resumable continuations.
**Gate:** behavioral parity with `loon run` on samples (F1); multi-shot
conformance tests.

### 3g — Fixpoint  → harness + `run-corpus.sh` modes
Compose `reader → expander → infer(+effects+ownership) → lower → vm`. Run the
self-hosted toolchain on the corpus and compare stdout to `loon run` (**F1**);
run it on its own `src/frontend/*.oo` source (**F2**). Wire **Stage-2's deferred
procedural macros** through the now-available evaluator.

## Risks / open questions

- **Performance.** Inference threads immutable `Subst`/maps; the EIR VM runs on
  the Rust EIR VM (double interpretation). Big web files may be slow; may need
  the same vector/`join` and char-vector tactics used in Stages 0–1, and
  possibly scoping the fixpoint corpus to `.oo` samples first.
- **State threading ergonomics.** No mutation on the VM: `Subst` (fresh vars +
  bindings) and lowering (`next-reg`, blocks) thread state as records/vectors,
  as the attachment pass did. Verbose but proven.
- **Differential granularity.** No `loon check --types` dump exists; checker
  differential is accept/reject + error-text parity, with principal types
  spot-checked on snippets.
- **`match` exhaustiveness / pattern typing** and **trait bounds** are sizable
  sub-problems inside 3b; may split further.
- **Scope.** This is multi-PR and the largest stage by far (~15k Rust LOC of
  surface). Sub-stages land and gate independently; v1 excludes WASM/native.

## Immediate next step

Begin **3a** (`types.oo`): the `Type` ADT + `Subst` + `unify`/`occurs`/
`generalize`/`instantiate` with unit tests, then a differential accept/reject
check against `loon check`. Stop at the 3a gate and report before 3b.
