# Elegance notes — sharp edges & opportunities

A living, prioritized list of changes that would make Loon (the language) and
its self-hosted compiler more elegant. Sourced from concrete friction hit while
building the self-hosted frontend (Stages 0–3f). "Breaking" is fine — it's our
language; each item notes whether it breaks existing code and rough effort.

Legend: **break?** = backward-incompatible · **effort** S/M/L.

## A. Language warts (worth breaking to fix)

1. **✅ FIXED — `=` is now structural.** Previously `=` in the EIR VM was heap
   identity for strings and aggregates, so `[= "ab" [str "a" "b"]]` was `false`
   and the reader had to build string equality from `len`+`index-of` (`streq`).
   The VM now compares strings, vectors, tuples, ADTs, sets, and maps by content
   (recursively), matching the legacy interpreter — see `val_eq` in `eir/vm.rs`.
   Follow-up: the `streq` workaround in `reader.oo` can now be replaced with `=`.

2. **✅ FIXED — interpolation is now `\(expr)` (Swift/Roc-style).** `{…}` used to
   be interpolation, so a literal `{` in a string needed escaping and brace-heavy
   text (maps, JSON, embedded Loon source) was painful — the worst fit for a
   brace-heavy, self-hosting language. Interpolation now uses `\(expr)` (reusing
   the escape char, matching Roc, which like Loon desugars to string concat), and
   **bare `{`/`}` are ordinary literal characters** — no escaping. `\{`/`\}`
   still produce literal braces for back-compat. See `unescape` (lexer) +
   `desugar_fmt` (parser). Migrated the interpolation tests and `word-freq.oo`;
   the self-hosted map/set tests dropped their `\{…\}` escaping. (Web-doc prose
   that teaches `{expr}` still needs a content pass — follow-up.)

3. **String indexing is O(index).** `char-at`/`substring` re-scan from the
   start, so naive scanning is O(n²); the reader works around this by splitting
   the whole source into a char vector once. **Fix:** O(1) indexable strings
   (or a documented cursor/bytes API). _break? no · effort M_

4. **Built-in vs declared effects are inconsistent.** `samples/effects.oo` uses
   `IO`/`Fail` with no `[effect …]` declaration (they're implicit), but user
   effects must be declared. **Fix:** either declare everything (with a `prelude`
   of standard effects) or document a fixed built-in set. _break? maybe · S_

4b. **✅ FIXED — user `fn` definitions now shadow builtins.** `lower_call`
   checked the builtin table before user functions, so `[fn sum …]` was silently
   ignored in favor of the builtin `sum`. It now resolves user functions first;
   builtins still resolve when not shadowed. (Operators and constructors keep
   priority.) See `lower_call` in `eir/lower.rs`.

4c. **✅ FIXED — constructor tags are globally unique.** Tags were assigned per
   type starting at 0, so the first constructor of every type shared tag 0;
   `match` (which lowers to a runtime tag-equality check) would match a pattern
   of one type against a value of another with the same ordinal — e.g. `[Some n]`
   vs a `[Cons …]` value — and bind garbage. `collect_ctors` now uses a global
   `next_tag` counter, so distinct constructors never collide. See `eir/lower.rs`.

4d. **✅ FIXED — maps are insertion-ordered.** `keys`, iteration, and display
   used to come back in hash order (non-deterministic): `[assoc [assoc {:x 1}
   :y 2] :z 3]` printed `{:z 3 :x 1 :y 2}`. The EIR VM's map is now an
   insertion-ordered persistent map (`OrdMap` in `eir/vm.rs`: an `imbl::HashMap`
   for O(1) lookup + an `imbl::Vector` of keys for order), so it prints
   `{:x 1 :y 2 :z 3}` deterministically — matching the self-hosted VM. (The
   legacy tree-walking interpreter still uses an unordered `imbl::HashMap`; a
   follow-up could align it.)

5. **`let` is a body statement, not an expression.** `[let x v]` scopes to the
   rest of the enclosing body — clean in sequences, but `[let …]` can't be used
   as a sub-expression (the lowerer/inferrer special-case it). **Consider:** a
   Clojure-style expression form `[let [x v …] body]` for composability, or keep
   the statement form but make it a true expression returning its body value.
   _break? depends · effort M_

## B. Missing builtins / stdlib

6. **✅ FIXED — `string → number` parsing.** `int` and `float` are typed
   `Str → Int`/`Str → Float` but their EIR VM impls ignored strings (returned
   `()`); they now parse (`[int "42"]` → `42`, whitespace trimmed, unparseable
   → `()`), keeping the int/float numeric conversions too. See `Built::Int` /
   `Built::Float` in `eir/vm.rs`. Follow-up: the self-hosted VM's hand-rolled
   `parse-int` can now defer to the native `int`.

7. **No structural equality / `Display` story.** `str`/`println` are variadic and
   ad-hoc; there's no `Show`/`Display` trait, so the VM reimplements value
   display. A principled `Display` (and `Eq`) trait would unify printing and
   comparison. _break? no · effort M_

8. **Numeric tower is fuzzy.** Inference treats arithmetic as `∀a. [a a -> a]`
   (a stand-in for a `Num` bound). Clarify Int/Float literals, coercion, and the
   `Num`/`Ord` traits so `+`/`<` are principled rather than special-cased.
   _break? maybe · effort M_

## C. Runtime / VM

9. **No working module `use` on the VM.** The entire self-hosted frontend is one
   concatenated file because cross-file `use` doesn't work on the EIR VM. This is
   the biggest structural limitation — real module support (even namespaced file
   concatenation) would let the compiler be organized into actual modules.
   _break? no · effort L · **highest structural impact**_

10. **⚙ PARTIAL — effect handlers were tail-resumptive only.** `resume` was an
    identity closure and the handler's return was spliced at the perform site,
    so there was no real abort and no resume-after-work. Now (EIR VM): `handle`
    delimits a prompt (body lowered as a thunk), `perform` captures the frame
    segment above the prompt as a one-shot `Obj::Continuation`, and the handler
    runs there with `resume := k`. This gives **real abort (0-shot → `try`
    works, was returning the wrong value)** and **resume-anywhere (1-shot, not
    just tail)**, one-shot enforced. Still TODO: **escaping continuations** (a
    continuation resumed *after* its `handle` has exited — needed for the
    function-passing `State` encoding and multi-shot); that needs a relocatable,
    self-contained continuation that re-establishes its prompt + handlers on
    resume (OCaml-5 fiber style), rather than the in-place frame-segment capture.
    A `Gensym`/`State`/`Ref` effect (#11) waits on that. _effort: L remaining_

11. **No ergonomic local mutation — but we have effects!** Every pass threads
    state by hand (reader counters, the type `Subst`, the lowering builder `LS`).
    This is inherent to purity, but a built-in `State`/`Ref` **effect** (handled
    by the VM) would let passes use mutable cells while staying pure-by-default —
    dogfooding the effect system. Depends on #10 for multi-shot. _break? no ·
    effort M (after #10)_

## D. Self-hosted compiler ergonomics

12. **Result/pair boilerplate.** The compiler defines many one-off carrier types
    (`UR`, `IT`, `Fr`, `RP`, `RPS`, `PR`, `Setup`, …) just to return "value +
    threaded state". Generic `Result a e`, `Option a`, and a real tuple
    `(a, b)` used functionally would remove most of them. _break? no · effort M_

13. **State-threading is verbose.** Pervasive `[match [step …] [RP x s1] …]`
    chains. A `do`-notation / state-monad macro (or the `State` effect from #11)
    would collapse these. _break? no · effort M_

14. **`match` exhaustiveness checking** would catch missing arms in the compiler
    itself (we hit bracket/arm bugs that a checker would flag). _break? no · M_

15. **Differential harnesses are bash.** `run-*-diff.sh` could be Loon programs
    once IO + process spawning exist, making the test rig self-hosted too.
    _break? no · effort M_

## E. Bigger bets

16. **Activate safety features by default.** Ownership existed but was dormant
    (only the REPL/tutorial path ran it); we just wired it into `loon check`.
    Audit for other dormant analyses; make `loon check` the single front door
    for *all* static checks (types + effects + ownership). _done for ownership ·
    effort S each_

17. **Optional param-mode annotations.** Ownership infers Borrow/MutBorrow/Move
    with zero syntax. Optional annotations (for docs/clarity, like `&`/`&mut`/
    `move`) could aid readability without changing inference. _break? no · M_

18. **Pattern matching: guards & or-patterns.** `[match x [Some n] when [> n 0] …]`
    and `[a | b]` patterns would make match far more expressive. _break? no · M_

## Suggested near-term order

1. `string→int`/`float` builtins (#6) — trivial, unblocks clean lexeme handling. **S**
2. Structural `=` for strings (#1) — removes the `streq` workaround everywhere. **M**
3. Interpolation sigil fix (#2) — stops brace-escaping pain in sources/tests. **S**
4. Multi-shot continuations (#10) → `State`/`Ref` effect (#11) — unblocks elegant
   state threading and the deferred `Gensym`. **L**
5. Module `use` (#9) — lets the compiler stop being one giant file. **L**
