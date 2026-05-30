# ARCHITECTURE.md — Current Rust Implementation (Stage 0 survey)

This document summarizes the **existing** Rust implementation of Loon as it
actually is today, to anchor the self-hosting effort. It covers the four
things the bootstrap plan needs pinned down — the surface AST/`Form` type, the
effect-row representation, the hygiene approach, and the codegen target — plus
**three divergences** between the plan's assumptions and the real code that
need a decision before Stage 0 work proceeds.

Everything below is grounded in the current tree under
`crates/loon-lang/src`. File:line references are to that crate.

---

## 1. Surface AST / `Form` type

The reader produces `Expr`, not a minimal `Form`. It is richer than the
plan's sketch (`symbol/keyword/number/string/list/vector`).

```rust
// ast/mod.rs:19
pub struct Expr { pub kind: ExprKind, pub span: Span, pub id: NodeId }

// ast/mod.rs:48
pub enum ExprKind {
    Int(i64), Float(f64), Bool(bool), Str(String),
    Keyword(String), Symbol(String),
    List(Vec<Expr>),                 // [head args...]
    Vec(Vec<Expr>),                  // #[a b c]
    Set(Vec<Expr>),                  // #{a b c}
    Map(Vec<(Expr, Expr)>),          // {:k v ...}
    Tuple(Vec<Expr>),                // (a, b)
    Quote(Box<Expr>),                // `expr
    Unquote(Box<Expr>),              // ~expr
    UnquoteSplice(Box<Expr>),        // ~@expr
    DotAccess(Box<Expr>, String),    // expr.field
}
```

Key facts:

- **Spans are byte-offset only** — `Span { start: usize, end: usize }`
  (`syntax/mod.rs:4`). No line/column. `merge` widens to cover children.
  Every node carries a span **and** a process-unique `NodeId(u32)`
  (`ast/mod.rs:9`) used as the key for type-checker side tables.
- **Numbers are split at read time** into `Int(i64)` / `Float(f64)`. Numeric
  literals may carry **unit suffixes** (`10m`, `5.0kg`) which the parser
  desugars to `[unit 10 :m]`.
- **Several "reader macros" are desugared in the parser, not a macro pass:**
  string interpolation `"hi {name}"` → `[str "hi " name]`; the postfix `?`
  operator → a `match` on `Ok`/`Err`; unit suffixes as above.
- **Comments are not in the AST.** `parse_with_comments` returns
  `(Vec<Expr>, Vec<Comment>)` where `Comment` carries a `Span`; the formatter
  re-attaches them by source position.
- A second, post-typecheck AST exists: `TypedExpr { kind, ty, span }`
  (`ast/typed.rs:8`) with a `TypedExprKind` that **drops** the quote/unquote
  variants (they're gone by then) and carries a resolved `Type`.

**Implication for Stage 0:** the shared `Form` type must be the *full* surface
set above (including `Set`, `Map`, `Tuple`, `DotAccess`, and the three quote
variants) if `read(fmt(x))` round-trips are to hold against the Rust corpus.
A reduced `Form` will not round-trip the existing `.oo`/`.loon` corpus.

---

## 2. Effect-row representation  ⚠️ diverges from the plan

The plan (Stage 3) assumes *"effect rows unify alongside type rows"* and a
single shared row representation. **The current implementation does not do
this.** There are two unrelated mechanisms:

- **Type-level rows** (for records) — true row types with a tail variable:
  ```rust
  // types/mod.rs:211
  Row(Vec<(String, Type)>, Option<TypeVar>)   // None = closed, Some = open
  ```
  unified structurally by `unify_rows` (`types/mod.rs:592`).

- **Effect rows** — *not* rows at all, but a plain set of names:
  ```rust
  // types/mod.rs:220
  pub struct EffectSet(pub BTreeSet<String>);   // union / subtract / subset
  ```
  attached via `Type::Effect(Box<Type>, EffectSet)` (`types/mod.rs:208`).

Crucially, **effect sets are *not* unified** through `unify`. The main
unifier has no real `Effect` case — effects are tracked by a separate
side-channel in the checker:

```rust
// check/mod.rs:86
pub fn_effects: HashMap<String, EffectSet>,   // per-function inferred effects
current_fn_effects: EffectSet,                // accumulator for current body
```

Inference = **accumulation**, not row unification: calling `Effect.op`
inserts the effect name into `current_fn_effects` (`check/mod.rs:2447`); calls
to other functions union their `fn_effects` (`check/mod.rs:2717`); `handle`
subtracts the handled effects (`check/mod.rs:2996`). There is *no* effect-row
tail variable, so effect polymorphism (a `compose` whose effect row is
inferred from its function arguments) is **not** representable today — the
homepage `compose` story is aspirational relative to the checker.

> **Decision needed (D1):** keep the existing `EffectSet` (set-of-names,
> accumulated) for the self-hosted checker — matching what exists — or move to
> real row-unified effect rows with a tail var (what the plan and homepage
> describe, a genuine semantics change). The single "shared effect-row
> representation" the plan wants does not exist yet, so Stage 0 cannot just
> import it; we have to choose what to define.

---

## 3. Hygiene approach  ⚠️ docs and code disagree

- **The docs claim hygiene:** `guide/macros.loon` — *"Loon macros are
  hygienic by default (like Scheme's `syntax-rules`)."*
- **The implementation has none.** `macros/mod.rs` is purely textual
  substitution. Bindings are `HashMap<String, Vec<Expr>>` keyed by raw symbol
  name (`macros/mod.rs:357`). There are no scopes, marks, or syntax objects.
  A `gensym` helper exists but is `#[allow(dead_code)]` and never called
  (`macros/mod.rs:95`). Variable capture is unprevented.

Expansion is top-down recursive (`expand_expr`, `macros/mod.rs:156`), re-
expanding macro output so macros-producing-macros work. Two macro flavors:
- **Template** macros — body is a `` `quote `` ``; expanded by `substitute`
  (`macros/mod.rs:409`) walking the template, replacing `~x` / `~@xs`.
- **Procedural** macros — body is arbitrary code, **evaluated at expansion
  time by the main interpreter** (`interp::eval_program`, `macros/mod.rs:571`)
  under a sandboxed effect allow-list (`COMPILE_SANDBOX`,
  `interp/mod.rs:38`). Allowed compile effects: `IO`, `Net`, `Env`, `Print`
  (`compile_builtins.rs`). So there is **already one shared evaluator** for
  macro bodies and runtime code — good, matches the plan's "don't fork two
  evaluators" constraint.

> **Decision needed (D2):** the plan says use scope-set hygiene *"unless
> ARCHITECTURE.md shows the existing expander uses another model — match what
> exists, flag if you'd change it."* What exists is **no hygiene**. So this is
> a genuine fork: (a) match the Rust expander's unhygienic textual model
> (cheapest, keeps differential tests byte-exact, but the docs become a lie),
> or (b) implement scope-set hygiene in the Loon expander (matches the docs'
> promise and the plan's torture-test gate, but then the Loon expander is
> *not* differentially identical to the Rust one on capture cases — by design).

---

## 4. Codegen target  ⚠️ runtime is one-shot, plan recommends multi-shot

Pipeline: `source → parse → macro-expand → typecheck → lower → EIR → backend`.

- **EIR** (`eir/mod.rs`) is a flat, SSA-ish, basic-block IR. `Op` is the
  instruction set (~17 variants: `Lit`, `Mov`, `Bin`, `Call`, `Invoke`,
  `Close`, collection builders, `Field`/`Tag`, and the effect ops
  `Perform`, `PushHandler`, `PopHandler`); blocks end in `Ret`/`Jmp`/`Br`/
  `Switch`/`Tail`/`Recur`/`Trap`.
- **Default backend is a register VM** (`eir/vm.rs`), invoked by `loon run`
  via `eir::vm::eval_eir`. Values are **NaN-boxed 64-bit** (`eir/value64.rs`):
  tags for heap ptr, inline int48, interned symbol, and immediates.
  Two other backends exist: **WASM** (`eir/wasm.rs`) and a partial
  **Cranelift native JIT** (`eir/native.rs`). The old tree-walking
  interpreter (`interp/`) is explicitly **superseded**, retained only for
  `--legacy` and the WASM/DOM bridge.
- **Effects at runtime = handler-passing / evidence-passing**, *not* CPS or
  stack-copying delimited continuations. Handlers are lowered to closures;
  `PushHandler`/`PopHandler` maintain a dynamic LIFO handler stack, and where
  the handler is statically known it's threaded as an **evidence** parameter
  for a direct call (`eir/lower.rs:1428`, `eir/vm.rs:622`).
- **Continuations are ONE-SHOT.** `resume` is a pre-allocated **identity
  closure** (`eir/vm.rs:140`): calling it returns the supplied value to the
  `perform` site and execution continues linearly. There is no reified,
  re-entrant continuation; the handler body runs once and the result flows
  back. Multi-shot resume (calling `resume` twice, backtracking,
  non-determinism) is **not supported by the current VM**.

> **Decision needed (D3 — the plan's flagged STOP):** the plan recommends
> **multi-shot** continuations and asks to confirm before codegen. The
> existing runtime is **one-shot evidence-passing**. Going multi-shot means a
> genuinely different runtime (reified resumable continuations / segmented or
> copied stacks), which the Rust VM does **not** have — so a multi-shot
> self-hosted compiler could not target the current VM unchanged, and the
> FIXPOINT gate (Loon compiler reproducing itself on the existing toolchain)
> gets much harder. One-shot keeps the self-hosted compiler aligned with the
> real VM.

---

## 5. Where this leaves Stage 0

The Stage 0 frontend (`src/frontend`, in Loon) can proceed **as soon as D1–D3
are decided**, because all three change what the *shared* definitions are:

- The shared `Form` type (§1) is well-defined and stable — safe to build now.
- The shared **effect-row** type (§2, D1) is a *prerequisite* the plan says
  Stage 0 declares and Stage 3 reuses; we must pick set-vs-row first.
- Hygiene (§2/D2) and continuations (D3) gate Stages 2 and 3, not Stage 0
  reading, but they shape the `src/frontend` data model (e.g. whether `Form`
  symbols need to carry scope-set slots) so they're cheaper to decide now.

**Recommended path (my opinion, for confirmation):**
1. **D1:** define a real **row-based effect representation** in Stage 0
   (fields = effect names, optional tail var) so the homepage `compose`
   example actually type-checks — but *also* keep a lowering to the Rust
   checker's `EffectSet` for differential testing. This is the one place I'd
   *improve* on the existing code rather than mirror it.
2. **D2:** implement **scope-set hygiene** in the Loon expander (honor the
   docs), and run the differential test against the Rust expander only on the
   hygiene-neutral corpus; treat capture torture-cases as Loon-only
   conformance tests. Carry a scope-set slot on `Form` symbols from day one.
3. **D3:** **one-shot** for the bootstrap, to keep the FIXPOINT reachable on
   the existing VM; revisit multi-shot as a follow-up once self-hosting holds.
   (This contradicts the plan's "recommend multi-shot" — flagging explicitly.)

These recommendations are deliberately conservative about the fixpoint and
honest about the doc/impl gaps. I want your call on D1–D3 before writing
`src/frontend`.

---

## 6. Decisions (confirmed) and Stage-0 outcome

**D1 — Effect representation: row-based, no bridge.** The self-hosted
frontend will define real effect rows (effect-name fields + an optional tail
variable) and infer them by unification, dropping compatibility with the Rust
checker's flat `EffectSet`. The homepage `compose` example becomes
expressible. (Differential parity with the Rust checker is intentionally
given up here.)

**D2 — Macro hygiene: scope sets.** The Loon expander will implement
Flatt/Racket-style scope-set hygiene, honoring the docs. `Form` symbols carry
a scope-set slot from Stage 0 onward (the `Vec` field on `FSym`) so later
stages extend `Form` rather than redefining it. Differential testing against
the (unhygienic) Rust expander will be limited to capture-neutral cases;
capture/​shadowing torture cases become Loon-only conformance tests.

**D3 — Continuations: multi-shot.** The compiler will target reified,
multi-shot resumable continuations. The current EIR VM is one-shot
(`resume` is an identity closure), so a multi-shot self-hosted compiler
cannot target today's VM unchanged; the Stage-3 FIXPOINT plan must account
for new runtime support (segmented/copied stacks or a CPS lowering). Flagged
as the largest risk to the byte-for-byte fixpoint.

### Stage 0 — Shared frontend (reader + Form): SHIPPED

Location: `src/frontend/reader.oo` (library), `src/frontend/tests/` (drivers),
`src/frontend/run-corpus.sh` (harness). Written in Loon, runs on the Rust VM.

- **Form** is the single shared surface type (`FInt FFloat FStr FKw FSym
  FList FVec FSet FMap FTuple FQuote FUnquote FSplice FDot FQuery FError`),
  every node span-carrying; `FSym` carries the D2 scope-set slot. Numeric
  literals keep their lexeme text (a reader does not interpret numbers).
- **Diagnostics are effects, not exceptions:** `Source.span` mints spans,
  `Reader.error` reports malformed input (resumed with an `FError`
  placeholder — error recovery, the reader keeps going).
- **Ship gate — round-trip read on the whole repo corpus: PASS.** All 67
  `.oo`/`.loon` files (samples, web docs, crates, docs). The gate is: the
  reader accepts the file (no `FError`) **and** the canonical form is an exact
  fixpoint, `canon(src) == canon(canon(src))`. Result:
  `ok=67 errored=0 unstable=0 empty=0`. Fidelity spot-checked: type decls,
  `match` with destructuring, pipes, nested closures, maps, keywords, and the
  large web pages all round-trip with top-level form counts preserved. 21
  inline unit tests also pass.

#### Runtime facts about the EIR VM that shaped Stage 0 (verified empirically)

These constrain every later stage too:

1. `=` is **heap identity** on strings (`[= "a" "a"]` is `false`); even
   `assert-eq` is bit-equality. String/char equality must be built from `len`
   + `index-of` (`streq`). Keywords, symbols, and ints compare by value.
2. The Rust lexer (which parses our Loon source) has **no `\r` escape** —
   `"\r"` becomes the two chars `\`,`r`. Source is LF; we never write `\r`.
3. A bare `{` in a string literal is a **parse error** (interpolation start);
   a literal open brace must be written `\{`. A bare `}` is fine.
4. **Multi-file `use` does not work** on the VM (`value is not callable`), so
   the frontend is one file; stages are concatenated, not imported.
5. `IO.list-dir` is unimplemented (returns Unit); `IO.read-file` works. The
   corpus list is generated by the harness, not enumerated at runtime.
6. `loon run` does **not** type-check, so the `Form` ADT's field-type
   annotations are runtime-irrelevant. `loon test` uses the *legacy*
   interpreter, so we validate via `loon run` (the real EIR VM) instead.
7. `fold` is `[fold init f coll]` (init first).

#### ⚠️ New finding — the Rust formatter is unsafe on brace-bearing strings

`loon fmt` rewrites the literal-brace escape `"\{"` to a bare `"{"`, which is
then **unparseable** — so `fmt` is not idempotent (it emits output it cannot
re-read) for any string containing a literal `{`. Consequently a brace-handling
reader **cannot be passed through the current `loon fmt`** without corruption,
and the Stage-0 source is therefore kept hand-formatted in canonical style and
**not** run through `fmt`. This is a concrete defect Stage 1's self-hosted
formatter must fix (and a caution for Stage 1's "byte-identical to the Rust
formatter" gate: the Rust formatter's own output is invalid here).
