# Verify: laws, a rewrite kernel, and an autoresearch loop

*October 2026 — design, nothing shipped*

Loon gets a small verification story that fits its thesis: invisible types,
effects as the security model, a VM that replays deterministically. No
dependent types, no tactic language. Three things, in order:

1. **`Div` as an effect** — nontermination is inferred like `IO` is. Pure
   total code is `#{}`; the kernel reasons only about that fragment.
2. **`law` + `proof` + a rewrite kernel** — equational logic with structural
   induction over ADTs. Proof objects are data; the kernel re-checks them.
3. **`loon verify`** — a harness that falsifies cheaply, proves what it can,
   and emits a JSON report an agent can loop on overnight (the autoresearch
   pattern: fixed harness, agent edits one directory, keep what scores).

What we take from Lean: a tiny trusted kernel, an untrusted elaborator (here,
the LLM), `sorry` as a tainting escape hatch, totality tiers. What we take
from Bend/Kind: proof by normalization, and the observation that Loon's
ownership discipline is already the proof a parallel backend would need.

## 0. Ground truth that shapes the design

- The prelude (`crates/loon-lang/src/prelude.rs`) is two ADTs. `reverse`,
  `map`, `concat`, `flatten` are Rust builtins (`builtins.rs`, kind
  `Special`) with no Loon body. **The kernel cannot unfold what has no
  definition.** So verification starts from a *reference prelude* written in
  Loon, and the builtins are checked against it by bounded evaluation.
- `loon run` does not gate on `loon check` (`docs/capabilities.md`). A
  verification story on top of a checker the runtime ignores is marketing.
  Wiring `check` into `run` is a prerequisite, not part of this plan.
- `[test name [params] body]` already exists (`interp/mod.rs`,
  `eval_test_def`). `law` sits next to it syntactically.
- The VM is deterministic and records/replays. A failing run is reproducible,
  which is what makes an overnight loop's signal trustworthy.

## 1. `Div`: nontermination as an effect

Koka's `div`. A function that may not terminate carries `Div` in its row.
Inference:

- Structural recursion on an ADT argument (every recursive call is on a
  strict subterm of a `match` scrutinee) → no `Div`.
- Any other recursion, or a call to a `Div` function, → `Div`.
- Builtins that can loop (`loop`, `while`, anything taking a callback it may
  call forever) are declared `#{Div}`.
- An explicit annotation can *add* `Div` (opt out of the check), never remove
  it.

Invisible for almost all code: the row is inferred, nobody writes it. The
payoff is a decidable fragment: `#{}` functions can be evaluated at check
time and unfolded by the kernel.

Lives in `check/`, next to effect-row inference. Error code in the E04xx
range.

## 2. Laws and proofs

### 2.1 The reference prelude

`verify/defs.oo`, frozen (content-hashed; a hash mismatch discards a run):

```loon
[fn append [xs ys]
  [match xs
    #[]            ys
    [cons x rest]  [cons x [append rest ys]]]]

[fn reverse [xs]
  [match xs
    #[]            #[]
    [cons x rest]  [append [reverse rest] #[x]]]]

[fn length [xs]
  [match xs
    #[]            0
    [cons _ rest]  [+ 1 [length rest]]]]

[fn map [f xs]
  [match xs
    #[]            #[]
    [cons x rest]  [cons [f x] [map f rest]]]]
```

A separate `verify/builtins.oo` holds *equivalence laws* between each
reference function and its builtin, e.g. `[law reverse-builtin [xs]
[= [reverse xs] [builtin-reverse xs]]]`. These are never proven, only
bounded-checked. They are the bridge: a proof about reference `reverse` says
something about the `reverse` people run only to the extent that bridge holds.

### 2.2 `law`

```loon
[law append-nil [xs] [= [append xs #[]] xs]]
[law append-assoc [xs ys zs]
  [= [append [append xs ys] zs] [append xs [append ys zs]]]]
```

Grammar: `[law NAME [PARAMS...] [= LHS RHS]]`. Params are universally
quantified; their types are inferred from use. Only `=` for now. Both sides
must be `#{}` (no `Div`, no other effects) or the law is rejected at parse
time with an error naming the offending call.

A law in a file is a *conjecture*. Its status is one of:

| status      | meaning                                                   |
|-------------|-----------------------------------------------------------|
| `falsified` | bounded evaluation found a counterexample (reported)       |
| `proven`    | a `proof` for it was kernel-checked with no `admit`        |
| `admitted`  | a `proof` was checked but contained `admit`                |
| `unknown`   | no proof, or the proof got stuck (stuck term reported)     |

### 2.3 `proof`

```loon
[proof append-nil
  [induction xs
    [nil  [unfold append]]
    [cons [unfold append] [rewrite ih]]]]
```

Grammar:

```
proof    := [proof NAME STEP*]
STEP     := [unfold FN]                ; one unfolding of FN at the leftmost-outermost redex
          | [rewrite RULE]             ; rewrite leftmost-outermost match, left to right
          | [rewrite RULE :rev]        ; right to left
          | [rewrite RULE :at N]       ; Nth match in preorder, 0-based
          | [induction VAR CASE*]      ; VAR must be a law param of ADT type
          | [cases VAR CASE*]          ; like induction, no new ih; VAR may be a
                                       ; constructor field bound by an earlier split
          | [refl]                     ; both sides syntactically equal after normalization
          | [admit]                    ; close the goal, taint the proof
CASE     := [CTOR STEP*]               ; CTOR is a constructor of VAR's type; nil/cons for Vec
RULE     := ih                         ; the induction hypothesis, when exactly one is in scope
          | ih-FIELD                   ; the hypothesis for recursive field FIELD
          | NAME                       ; a law already proven earlier in dependency order
```

Semantics, per goal `[= L R]`:

- The kernel keeps a current goal. Each step transforms `L` (never `R`;
  symmetric steps are expressed with `:rev`).
- `unfold FN` replaces the leftmost-outermost application of `FN` by its
  body with arguments substituted, then normalizes `match` on a constructor
  head. If no such application exists, the step fails.
- `rewrite RULE` instantiates `RULE`'s free variables by matching its LHS
  against a subterm of the goal's `L`; replaces it with the instantiated RHS.
  Matching is first-order, syntactic, up to normalization of constructor
  `match`. No unification through function bodies.
- `induction VAR` splits the goal per constructor. In a recursive
  constructor's case, `ih` is the law instantiated at the recursive subterm.
  The case's fields get fresh names (`x`, `rest`, `rest1`, ... avoiding names
  already in the goal), and there is one hypothesis per field whose declared
  type is the ADT itself, cited as `ih-FIELD` (e.g. `[Add a b]` gives
  `ih-rest` and `ih-rest1`). Plain `ih` works only when exactly one is in
  scope. Hypotheses stay universally quantified over the law's other params,
  so `ih-rest1` may be used at a different stack than the goal's. Stuck
  reports list the hypotheses in scope. A field like `(List Rose)` inside
  `Rose` mentions the ADT but gets no hypothesis.
- `cases VAR` works on law params and on constructor fields from an earlier
  split. A field's type comes from the constructor's declaration with the
  ADT's type parameters instantiated, so the head of a `List Instr` param is
  an `Instr` and can be split into `Push`/`IAdd`. Law param types come from
  verify's own HM inference over defs.oo (`verify/infer.rs`), falling back
  to the language checker. Note this inference is effectively trusted: a
  split over the wrong ADT would be unsound, as it already was with the
  checker-derived types. It is small and fails closed (no type, no split).
  Only legal if every function mentioned in the goal is `#{}` — this is where
  `Div` does its work.
- A goal closes when `L` and `R` are syntactically equal after normalization
  (`refl` is implicit at the end of every case; writing it is allowed).
- A proof may cite a law as `RULE` only if that law is `proven` or `admitted`
  *earlier in dependency order*. Citing an `admitted` law makes the citing
  proof `admitted`. Cycles are rejected.

AC declarations (a top-level form in `laws/*.oo`):

```
ac       := [ac OP ASSOC-LAW COMM-LAW]
          | [ac OP ASSOC-LAW COMM-LAW :unit UNIT UNIT-LAW]
```

e.g. `[ac add add-assoc add-comm :unit Z add-zero-right]`. Rules:

- `OP` must be a two-argument fn of defs.oo. The kernel checks each cited
  law's statement for shape, up to renaming of its params and either
  orientation: ASSOC-LAW `[= [OP [OP a b] c] [OP a [OP b c]]]`, COMM-LAW
  `[= [OP a b] [OP b a]]`, UNIT-LAW `[= [OP n UNIT] n]` or
  `[= [OP UNIT n] n]`, with `UNIT` a nullary constructor or a literal. A
  malformed declaration is reported in `errors` and ignored.
- The declaration counts as citing its laws, in every proof whose law (or a
  law it cites) mentions `OP` or a fn reaching `OP`, except the proofs the
  cited laws themselves transitively rest on: those never see the
  normalization. It is active for a proof only once all its laws are
  `proven` or `admitted`; an `admitted` one makes the proof `admitted`.
- When active, normalization canonicalizes `OP` applications like the
  builtin `+` (which is always on, with literal folding and 0 as unit):
  nested applications are flattened, `UNIT` operands dropped, operands
  sorted by printed form and rebuilt right-nested; one operand stands
  alone, none gives `UNIT`. `rewrite` works on that canonical form (rule
  sides are normalized the same way, and an `OP` node in a rule's LHS also
  matches with its two operands swapped), so operand order in stuck
  reports and `:at` counts is printed-form order.

That is the whole trusted surface: substitution, one-step unfolding,
first-order matching, constructor-`match` normalization, and the induction
scheme for ADTs. Target size: a few hundred lines in `check/verify.rs`.
Everything clever lives outside it.

### 2.4 Worked example

Full trace for `append-nil`, cons case. Goal after `induction xs`, case
`[cons x r]`, with `ih : [= [append r #[]] r]`:

```
L = [append [cons x r] #[]]
unfold append  → [match [cons x r] #[] #[] [cons x rest] [cons x [append rest #[]]]]
               → [cons x [append r #[]]]          ; match normalized
rewrite ih     → [cons x r]
R = [cons x r]                                    ; closed
```

A proof that needs lemmas:

```loon
[proof rev-append                 ; [= [reverse [append xs ys]] [append [reverse ys] [reverse xs]]]
  [induction xs
    [nil  [unfold reverse] [unfold append] [rewrite append-nil :rev]]
    [cons [unfold reverse] [unfold append] [rewrite ih] [rewrite append-assoc]]]]

[proof rev-rev                    ; [= [reverse [reverse xs]] xs]
  [induction xs
    [nil  [unfold reverse]]
    [cons [unfold reverse] [rewrite rev-append] [rewrite ih]
          [unfold reverse] [unfold append]]]]
```

When a proof is stuck the report says where:

```
unknown  rev-rev  case cons: stuck at
  L = [reverse [append [reverse r] #[x]]]
  no step applies
```

The stuck term is the signal. It names the missing lemma.

## 3. `loon verify`

```
loon verify [DIR]        # default: verify/
  --json                 # machine report (the agent reads this)
  --bound N              # bounded-evaluation input size, default 6
  --seed S               # for the generator; default fixed
```

Directory layout, with what is writable by an agent in the loop:

```
verify/
  defs.oo        frozen   reference prelude
  builtins.oo    frozen   bridge laws (bounded-only)
  canary.oo      frozen   [law canary [] [= 1 2]] and friends
  laws/          agent    conjectures
  proofs/        agent    proof objects
```

Pipeline per run:

1. Parse everything; reject any law whose sides are not `#{}`.
2. **Bounded evaluation** of every law: generate inputs up to `--bound`
   (exhaustive for small ADTs, seeded random for `Int`/`Str`), evaluate both
   sides in the EIR VM, report the smallest counterexample. Laws falsified
   here are *not* sent to the kernel.
3. **Kernel** for every surviving law with a `proof`, in dependency order.
4. **Canary check**: if any canary law is `proven` or `admitted`, the whole
   run is reported `unsound` and scored 0.
5. **Report** — human text, or JSON:

```json
{
  "run": "2026-10-28T03:14:00Z",
  "defs_hash": "sha256:…",
  "sound": true,
  "laws": [
    {"name": "append-nil",  "status": "proven"},
    {"name": "rev-append",  "status": "falsified",
     "counterexample": {"xs": [1], "ys": [2]}, "lhs": [2,1], "rhs": [1,2]},
    {"name": "rev-rev",     "status": "unknown",
     "stuck": {"case": "cons", "term": "[reverse [append [reverse r] #[x]]]"}}
  ],
  "score": {"proven": 1, "admitted": 0, "falsified": 1, "unknown": 1, "value": 1}
}
```

Score: `proven - admitted * 2`, and `0` when `sound` is false. A
`falsified` law is not penalized — a counterexample is a result. The
agent's incentive is to turn `unknown` into `proven`, or into a corrected
conjecture.

## 4. The autoresearch loop

Karpathy's pattern, unchanged: a fixed harness, an agent that edits one
directory, run, keep if better, repeat.

```
loop:
  agent reads report.json
  agent edits verify/laws/ and verify/proofs/      (nothing else)
  loon verify --json > report.json
  if report.sound and report.score > best:
      commit; best = score
  else:
      git checkout -- verify/laws verify/proofs
```

What the agent does with each status:

- `falsified` → fix the conjecture (the counterexample says how) or drop it.
- `unknown`, stuck → the stuck term names a lemma; conjecture it, prove it,
  retry.
- `unknown`, no proof → write one. Start with `induction` on the first
  param of ADT type; that is right surprisingly often.
- `proven` → leave it. Proven laws are rewrite rules for later proofs and,
  eventually, for the optimizer.

Why the score is trustworthy:

- `defs.oo`, `builtins.oo`, `canary.oo` and the kernel are not in the
  writable set, and `defs_hash` is in the report. The agent cannot weaken
  what it is scored against.
- `admit` is visible and taxed. It never produces `proven`.
- The canary cannot be proven by a sound kernel. If it is, the run is thrown
  out and the kernel bug is the night's most valuable output.
- Every run is deterministic (fixed seed, EIR VM); a surprising result
  replays.

### 4.1 Conjecture mining (later)

QuickSpec-style: enumerate terms up to depth 3 over `defs.oo` functions and
a few variables, bucket them by evaluation on random inputs, emit `[law …]`
for each pair in a bucket. Bounded evaluation prunes the lies; the agent
proves the rest. This is what turns "prove what I wrote" into "find what is
true". Not needed to start the loop; hand-written seed laws are enough.

## 5. Order of work

1. **`Div` inference** in `check/`. Small, self-contained, has value on its
   own (the checker can evaluate `#{}` code at compile time).
2. **Reference prelude** `verify/defs.oo` + bridge laws. Mostly writing Loon.
3. **`law` form**: parse, type, reject non-`#{}` sides.
4. **Bounded evaluation** of laws in the VM. This alone is useful
   (property testing with counterexamples) and ships before any kernel.
5. **Rewrite kernel**: `unfold`, `rewrite`, `induction`/`cases`, `refl`,
   `admit`, dependency order, canary. `check/verify.rs`.
6. **`loon verify --json`**.
7. **The loop script** (`scripts/autoresearch.sh` or a Loon program using
   `Process.exec`), and a first overnight run against list laws.
8. Conjecture miner.
9. Capability certificates: embed the checked effect rows and grant table in
   `loon image` so fxos re-checks them at load. Separate plan; same spirit.

## 6. Not doing

- Dependent types, refinement types, SMT. Conflicts with invisible types and
  would make the trusted surface large. SMT may return as an optional
  *bounded* backend later; it never becomes the kernel.
- A tactic language. The LLM is the tactic engine; the kernel only checks.
- Laws over effectful code. If it is not `#{}`, bounded evaluation under
  record/replay is the tool, not proof.
- Proving builtins correct. They are *bridged* by bounded laws, and that
  gap is stated in every report (`builtins.oo` laws are listed with status
  `bounded` and never `proven`).

## 7. Open questions

- `rewrite` position selection. `:at N` is crude; Lean's `rw … at h` and
  `simp` lemma sets are where this goes if proofs get long. Decide after the
  first overnight run shows what proofs actually look like.
- Induction on non-first params, nested induction, mutual recursion in
  `defs.oo`. Reject initially; expand when a stuck proof demands it.
- Should `law` live in ordinary `.oo` files next to `test`, or only under
  `verify/`? Leaning ordinary files, with `loon verify` collecting them, so
  a library ships its laws.
