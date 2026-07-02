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

## 2. Effect-row representation  ✅ now row-based (updated)

*(This section originally documented a divergence: effects were a flat
`EffectSet` accumulated in a checker side-channel, with no tail variable and
no unification. That is no longer true — the effect-rows track landed
row-based inference in the Rust checker.)*

The plan's assumption — *"effect rows unify alongside type rows"* — **now
holds.** Both row mechanisms exist and are structurally analogous:

- **Type-level rows** (for records) — row types with a tail variable:
  ```rust
  Row(Vec<(String, Type)>, Option<TypeVar>)   // None = closed, Some = open
  ```
  unified structurally by `unify_rows` (types/mod.rs).

- **Effect rows** — real rows with an optional tail variable:
  ```rust
  pub struct EffectRow {
      pub labels: BTreeSet<String>,   // concrete effect labels
      pub tail: Option<TypeVar>,      // None = closed, Some = open
  }
  ```
  carried on function types as `Type::Fn(params, ret, EffectRow)` and as
  `Type::Effects(EffectRow)` for tail-variable bindings, unified through
  the main `unify` via `unify_effect_rows` (types/mod.rs), with an occurs
  check for infinite rows.

The checker still keeps `fn_effects: HashMap<String, EffectRow>` and an
ambient `current_fn_effects: EffectRow` (check/mod.rs), but inference is now
**row unification**, not accumulation: performs push labels through the
ambient row's tail; calls link the callee's instantiated row into the
ambient row; `handle` subtracts handled labels and constrains the body row's
tail so handled effects cannot leak through it. The row tail generalizes
with let-polymorphism, so effect polymorphism (a `compose`/`twice` whose
effect row is inferred from its function arguments) **is representable** —
the homepage `compose` story now type-checks. Row mismatches surface as
`E0403`.

> **D1 status:** the decision below predates this change. A row-based
> self-hosted checker can now aim for **differential parity with the Rust
> checker on effect inference** — the "no bridge / parity given up"
> rationale no longer applies.

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
variable) and infer them by unification. The homepage `compose` example
becomes expressible. *(Update: the Rust checker has since moved to the same
row-based representation — see §2 — so the original caveat "differential
parity with the Rust checker is intentionally given up" no longer applies;
parity on effect inference is back on the table.)*

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

### Stage 1 — Formatter (self-hosted, faithful)

Location: `src/frontend/formatter.oo` (concatenated after `reader.oo`).
A Wadler/Leijen pretty-printer (`Doc = DNil | DText | DLine | DHard |
DIndent | DConcat | DGroup`) with a cons-list-stack renderer, mirroring the
Rust formatter's per-form layout (generic lists, `fn`/defn, `let`, `if`,
`match` pair-per-line, `pipe`, `type`/`effect`, vec/set/map/tuple) at
2-space indent / width 80.

**Deliberately faithful, NOT byte-identical to `loon fmt`.** On inspection the
plan's **gate 2 (faithful round-trip) and gate 3 (byte-identical to the Rust
formatter) conflict** on the corpus, because `loon fmt` is lossy:

- **Floats:** rendered via Rust `Display`, so `5.0`→`5` (re-reads as an
  **int**) and `3.140`→`3.14` (verified empirically).
- **Literal braces:** `"\{"`→`"{"`, unparseable (the brace bug above).
- **Interpolation:** `"hi {x}"` is desugared to `[str "hi " x]` and printed
  that way — the original literal is gone.
- **Blank lines / comments:** preserved from the source buffer via a
  position-based attachment pass the reader does not reproduce.

A formatter satisfying gate 2 (`read(fmt x) ≡ read x`) therefore *cannot* be
byte-identical to one that rewrites `5.0`→`5`. The self-hosted formatter keeps
float lexemes, literal braces, and interpolated strings, targeting gates 1
(`fmt(fmt x)==fmt x`) and 2 (`canon(fmt x)==canon(x)`), both **PASS 65/65** on
the corpus (+ 21 inline).

#### Comment + blank-line preservation (gate-3 closure)

`src/frontend/comments.oo` ports the Rust formatter's comment machinery: a
string-aware comment scanner plus `build_attachments`/`attach_seq`/`attach_into`
(leading / trailing / dangling per node, `blank_before`, program leading/
trailing), keyed by an encoded span. `formatter.oo` is now comment-aware
(`trailing_doc`/`body_break`/`close_after`/`fmt_children` and the special
`match`/`map` printers), and `format_program` emits program-leading/trailing
comments, per-form leading/trailing, and source blank lines.

Result: **byte-identical to the float-fixed Rust formatter on every corpus file
free of parser-level desugaring** — full corpus **45/65** (`samples/*.oo` went
from 8/21 layout-only to 19/21). Every one of the 20 remaining diffs is a
*parser* divergence, not a formatter or comment/blank one:

- **String interpolation** — `"…{x}…"` → the parser desugars to `[str …]`.
- **Literal braces** — source `\{` → the parser's unescape+desugar collapses it
  to a bare `{`; the faithful reader keeps `\{` (which round-trips). This is the
  brace bug from §6: Rust's bare-`{` output does not re-parse.
- **Unit literals** — `100.0m` → the parser desugars to `[unit 100.0 :m]`.

(The 18 web `.loon` files differ only via interpolation / literal braces, which
saturate HTML-templating source.) Closing these needs the parser to preserve
surface syntax (or the reader to replicate its desugaring) — out of formatter
scope, and in conflict with gate 2 wherever the Rust desugaring is lossy.

Known edge: a comment placed immediately before a close bracket (dangling) is
where the Rust formatter has a latent bug (it lets `]` share the comment's line,
producing non-round-trippable output) and the EIR VM additionally shows
attachment non-determinism for it; no real corpus file exercises this and the
corpus round-trip gate is 65/65.

#### Gate 3 — progress (decision: fix Rust fmt, then match)

Step 1 done: the Rust formatter's **float defect is fixed** —
`crates/loon-lang/src/fmt/mod.rs` now renders `Float` with `{:?}` so `5.0`
stays `5.0` (re-reads as a float, not an int) instead of `5`. Idempotent;
full Rust suite still 488/0.

**Measured Loon-vs-fixed-Rust byte match on `samples/*.oo`: 8 identical, 13
differ — and every diff is blank-lines or comments, never layout.** That is,
the self-hosted formatter's *layout* (indentation, group/break, and all
special-form rules) is already byte-identical to the Rust formatter; the only
remaining gate-3 gaps are **metadata the Stage-0 reader drops by design**:

1. **Blank-line preservation** — Rust keeps source blank lines via a
   position-based `blank_before` set; the Loon side only inserts blanks
   between `fn`/`type`/`effect` defns. (Smaller fix: have the reader record
   ≥2-newline gaps and thread them to the formatter.)
2. **Comment preservation** — the reader discards comments; Rust attaches them
   (leading/trailing/dangling) and re-emits them. (Larger: port the
   comment-attachment pass.)

Still parser-level (out of formatter scope): literal-brace round-trip (the
`unescape`/desugar `\}`→`}}` interaction) and interpolation (`"hi {x}"` is
desugared in the parser before the formatter sees it). Gate 3 is therefore
**substantially closed (layout-identical)**; full byte-identity is itemized
above as reader-metadata + parser work.

### Stage 2 — Macro expander (core): quasiquote + scope-set hygiene

Location: `src/frontend/expander.oo` (concatenated after `reader.oo`).

Done and tested (12/12 inline, 6/6 differential, 65/65 corpus):
- **Quasiquote / unquote / unquote-splicing** as a `Form -> Form` engine with
  nesting levels (nested `` ` `` raise the level; `~`/`~@` fire at level 0).
- **Template macros** `[macro name [params] `body]`, including `&rest`
  parameters, an argument used multiple times, **nested macros**, and
  **macro-generating-macro** (output is re-expanded to a fixpoint); expansion
  descends into sub-forms and is identity (modulo layout) on macro-free source.
  Substitution matches the Rust expander exactly: a bound parameter is
  substituted even when **bare** (no `~`); `~name` bound to many values becomes
  a `List`; unbound `~` / names pass through unchanged.
- **Scope-set hygiene (D2), expander side:** every identifier a macro
  *introduces* gets the macro use's fresh scope (carried in the `FSym`
  scope-set slot reserved in Stage 0); identifiers coming from the macro's
  *arguments* are untouched. Verified: expanding `[cap tmp]` where `cap`
  introduces its own `tmp` yields a scoped introduced `tmp` and an unscoped
  argument `tmp` — the no-capture mechanism. (Binding *resolution* over scopes
  is the Stage-3 resolver's job; hygiene is only *observable* through name
  resolution, so the end-to-end no-capture guarantee lands in Stage 3.)
- **Effects:** expansion-time diagnostics via `Expand.error` only. The plan's
  `Gensym` effect is replaced by deriving each macro use's scope from its
  **call-site span start** (unique per source position): the EIR VM cannot
  keep handler state across one-shot resumes, so a stateful `Gensym` handler
  would hand out a constant — span-derived scopes need no state. A threaded
  counter is the alternative if globally fresh ids are later required.

- **Procedural macros** `[macro name [params] #{effects} body]` are
  **recognized** (the body sits after an effect set) and carried as
  `style :procedural`, so they are never mis-parsed as templates. Their
  *execution* — running the arbitrary Loon body at expansion time — is
  **deferred**: it needs the tree-walking evaluator (shared with Stage 3) and
  there is no `eval` builtin on the EIR VM to borrow. A procedural call expands
  to an `Expand.error` placeholder rather than wrong output. Implementing it is
  Stage-3 work (the evaluator) plus the AST-as-value map protocol
  (`{:kind :symbol :name …}`) and the compile-effect sandbox.

**Differential test vs the Rust expander** (`run-differential.sh`,
`tests/macros/*.oo`): each program uses macros and prints deterministic output;
we compare `loon run PROG` (Rust expands + runs) against self-hosted-expand →
write macro-free source → `loon run`. **6/6 behaviorally equivalent** (simple,
rest+splice, nested, macro-generating-macro, arg-used-twice, bare-substitution).
A behavioral diff cannot exhibit hygiene (scopes don't serialize), so hygiene is
asserted at the Form level instead (inline). The expander is also a verified
no-op on the whole corpus (`run-corpus.sh expand`: **65/65**, 0 changed).
