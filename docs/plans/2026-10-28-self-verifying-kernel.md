# The kernel proves itself

*October 2026 — design and ladder; companion to `2026-10-28-verify-autoresearch.md`*

## The claim

Model the proof kernel in Loon, define what it means for a law to hold, and
prove in Loon that every proof object the model accepts is of a law that
holds. Then run that proof through `loon prove`.

It is circular in the way every self-verified checker is (Milawa, 2009, did
it by hand). The payoff is not an escape from trust but a shrinking of it:
today trust is "read `crates/loon-lang/src/verify/*.rs`". After, trust is
"the Loon evaluator agrees with `verify/meta/defs.oo` on one file", and
every soft spot in the Rust kernel (AC canonicalization, commutative
matching, inferred field types in `cases`) becomes a lemma that either goes
through or names the bug.

Nobody has done this where the proofs come from search rather than a human.

## The model

First-order terms, no lambdas, so no beta. Everything structural.

```loon
[type Term [V Sym] [C Sym (List Term)] [A Sym (List Term)]]
;; variable, constructor application, function application

[type Fn [Def Sym (List Sym) Term]]            ; name, params, body (a match tree is a Term)
[type Prog [Prog (List Fn)]]
[type Subst [Subst (List (Pair Sym Term))]]

[fn subst   [t s]]                              ; capture-free: no binders in Term
[fn matcht  [pat t]]                            ; Option Subst, first-order
[fn step    [prog t]]                           ; one leftmost-outermost reduction
[fn evaln   [n prog t]]                         ; n steps, structural on n (the fuel)
```

A law `[= L R]` with params `xs` *holds* in `prog` when for every closing
substitution σ and every fuel n large enough, `evaln n prog (subst L σ)` and
`evaln n prog (subst R σ)` reach the same normal form. Fuel keeps every
function in the model structurally recursive; "large enough" is quantified
by a law param, so no analysis sneaks in.

Proof objects are the ADT the Rust kernel already parses, re-stated:

```loon
[type Step [Unfold Sym] [Rewrite Rule Bool Nat] [Induction Sym (List Case)] [Cases Sym (List Case)] [Refl] [Admit]]
[type Case [Case Sym (List Step)]]
[type Rule [Ih Sym] [Lemma Sym]]
[fn check [prog lemmas goal steps]]             ; the model kernel: Bool
```

Soundness, the thing to prove:

```
check prog lemmas goal steps = T
∧ every lemma holds
∧ no Admit in steps
→ goal holds
```

That is an implication, so **hypotheses in laws** come first (rung 1).

## The ladder

Each rung is a `verify/meta/` corpus addition with its own `loon prove`
run, so there is a demo at every step.

0. **Done today**: the Rust kernel, `loon prove`, 22 laws, AC closing.
1. **Hypotheses in laws.** `[law name [params] :given [[= a b] ...] [= L R]]`.
   Hypotheses are rewrite rules available only inside that proof; citing
   a hypothesis-carrying law instantiates its hypotheses as obligations
   that must themselves be proven or match an in-scope hypothesis. Kernel
   change in `verify/kernel.rs` and `mod.rs`; grammar in the main plan.
2. **Substitution lemmas** (no hypotheses needed). `subst (subst t s1) s2 =
   subst t (compose s1 s2)`, `subst t empty = t`, and the match soundness
   trick stated unconditionally: `apply-match p t = t` where `apply-match`
   substitutes the match result back into `p` and returns `t` on failure.
3. **Evaluation lemmas.** Fuel monotonicity: once a term reaches normal
   form at fuel n it stays there at n+k. Unfold correctness: one `Unfold`
   step on a redex is one `step` of the evaluator.
4. **Rewrite soundness.** If a rule holds, rewriting a subterm with it
   preserves holding. Needs rung 1 and a congruence lemma: evaluation
   respects subterm replacement.
5. **Induction soundness.** For the ADTs in the model program, the case
   split plus hypotheses is the structural induction principle. Needs
   well-founded induction in the Rust kernel to even state (induction on
   proof objects is structural; induction *about* induction over an
   arbitrary ADT needs a size measure).
6. **The theorem.** Soundness of `check` by structural induction on steps,
   assembling rungs 2 to 5.
7. **Close the loop.** Encode the proof of rung 6 as a proof object the
   model accepts; run `loon prove` on it; publish the two hashes.

Deliberately out of scope for the model, and therefore admitted gaps in the
self-check: AC canonicalization, commutative matching, the HM in
`infer.rs`. Each gets a rung later or stays a stated trust assumption. The
first version of the theorem is about the *core* kernel: unfold, rewrite,
induction, cases, refl.

## What the Rust kernel must grow

- Hypotheses in laws (rung 1).
- `Pair` and `Option` in `defs.oo`-style ADTs; `Bool` as `[type B T F]`.
- Well-founded induction, or a size-measure induction principle (rung 5).
- Nested structural recursion through `List Term` (a `Term` holds a list of
  `Term`s): the totality checker must accept recursion through `map`-like
  helpers, and `induction` on `Term` must supply hypotheses for the elements
  of a `List Term` field. Today a field like `(List Rose)` gets no
  hypothesis. Rung 2 will hit this on the first lemma.

## Rung 2, as built

`verify/meta/` (`loon prove verify/meta`): `defs.oo` declares `Term`, `Sym`
(naturals), `Pair`, `Option`, `B` (`T`/`F`), `Subst` (an association list),
and `subst`/`subst-list`, `apply-var`, `map-rhs`, `compose`, `term-eq`/
`terms-eq`, `matcht`/`match-list` (non-linear: a repeated variable must
match equal terms; since rung 3 the binding list is threaded left to right
by `match-in`/`match-list-in`, like the Rust `match_term`), `apply-match`.
Proven: `subst-empty` and `subst-compose`, each with its list version,
plus helper lemmas (`nat-eq-refl`, `sym-eq-refl`, `subst-if`,
`subst-apply-var`) and `apply-match-var` (match soundness for a variable
pattern). `compose s1 s2` is `s1`'s bindings with `s2` applied, then all of
`s2`'s; lookup takes the first binding, which gives the usual
composition.

Open at rung 2 (closed in rung 3, below): `apply-match` in general. After the constructor test the goal is
`[match [if-b [sym-eq f g] [match-list ps ts] None] ...]`: it needs
`sym-eq f g = T → f = g` and `match-list ps ts = Some s → subst-list ps s
= ts`, implications, i.e. rung 1's `:given` plus case analysis on a
non-variable scrutinee (`cases` only splits variables today).

Kernel growth it forced:

- **Totality of mutually recursive groups.** The call graph's strongly
  connected components are checked together: a group is total when its
  outside callees are total and some choice of one *measure* param per member
  makes every call between members pass, in the callee's measure position, a
  pattern-bound strict subterm of the caller's measure param. The measure
  argument's size strictly decreases along every call chain in the group.
  For one fn this is the old rule. `subst` (measure `t`) calls
  `subst-list` on the `ts` field; `subst-list` (measure `ts`) calls `subst`
  on the head and itself on the tail.
- **Mutual induction.** `[proof P :mutual Q [induction t ...]]` and
  `[proof Q :mutual P [induction ts ...]]` (or `:mutual [Q R ...]`) form a
  group, declared identically by every member. Each member's proof must be
  one `induction` on a param of fully known type (since rung 3 members may
  carry `:given` hypotheses; a partner's `ih-FIELD` carries them as
  obligations).
  In an induction case of P, a constructor field whose type is exactly Q's
  induction type gets Q at that field as `ih-FIELD` (Q's other params
  quantified, renamed apart); P's own `ih` is as before. Sound by strong
  induction on value size: each case uses a group law only at a constructor
  field of the value being split. The driver makes no member citable (nor
  any proof citing one checkable) until every member closed, and marks all
  unproven if any is stuck. A false pair (`subst t s = t` with its list
  version) fails: the list half closes relative to the term half, the term
  half cannot.
- **Inference of mutually recursive fns.** `infer.rs` types each component
  together (monomorphic inside, generalized after), so laws over `subst`
  get param types and bounded inputs.
- The bounded generator already enumerated `Term`, `Subst` and `(List (Pair
  Sym Term))` by size; redeclaring `Option` in defs.oo does not clash with
  the evaluation program's prelude (no renaming needed).

For rung 3: a `Fn`/`Prog` model with `step` and fuel-indexed `evaln`;
`cases` on a non-variable scrutinee (or a `B`-valued lemma split), which
`apply-match` needs too; and `:given` (rung 1) inside mutual groups, which
the mutual scheme refuses today.

## Rung 3 (kernel half), as built

Kernel: `[cases EXPR :as NAME ...]` on any term of ADT type built from total
fns, `[rewrite RULE :in HYP]`, `[sym]`, ex falso on a hypothesis equating
distinct constructors, and `:given` inside `:mutual` groups (grammar and
soundness in the main plan, section 2.3).

Model change: matching now threads one binding list (`match-in p t bs`,
`match-list-in ps ts bs`, `bind-var`), which is how the Rust kernel
matches; `matcht`/`match-list` are `wrap-subst` of it from `Nil`. The
merge-of-two-substitutions version needed a domain/union theory that
ballooned; the threaded one only ever prepends a binding for an unbound
variable. Still non-linear: a bound variable must meet an equal term
(`term-eq`).

`apply-match` is proven. Chain: `nat-eq-sound`, `sym-eq-sound`,
`nat-eq-comm`, `sym-eq-comm`; `term-eq-refl`/`terms-eq-refl` and
`term-eq-sound`/`terms-eq-sound` (mutual, the latter conditional);
`lookup-apply-var`; `agree` (`b` reads like `b0` at every key of `ks`) with
`agree-refl`, `agree-lookup`, `agree-cons-fresh`; `match-in-mono` (mutual,
conditional: agreement with the result gives agreement with the input);
`match-in-sound` (mutual, conditional: any `b` agreeing with the result
maps the pattern to the term); then `apply-match` is a `cases` on
`[match-in p t Nil]` and `match-in-sound` with `agree-refl` discharging the
agreement. The evaluator lemmas `evaln-succ-normal`, `evaln-add`,
`fuel-monotone` are proven with `sym` and `cases [step p t] :as o`.

## Risks, said plainly

- The model kernel will not be the Rust kernel. It is a re-statement. The
  honest claim is "the Rust kernel implements the model", checked by running
  both on the same corpus and comparing verdicts, not by proof.
- Fuel-based evaluation makes "holds" a statement about all large fuels. The
  lemmas get a `fuel` param everywhere. Tedious, not deep.
- Nested induction (Term through List Term) is where the current totality
  checker and induction rule are weakest. Expect rung 2 to be mostly kernel
  work.
