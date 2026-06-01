# Elegance notes — sharp edges & opportunities

A living, prioritized list of changes that would make Loon (the language) and
its self-hosted compiler more elegant. Sourced from concrete friction hit while
building the self-hosted frontend (Stages 0–3f). "Breaking" is fine — it's our
language; each item notes whether it breaks existing code and rough effort.

Legend: **break?** = backward-incompatible · **effort** S/M/L.

## A. Language warts (worth breaking to fix)

1. **`=` is heap identity for strings.** The reader has to build string equality
   from `len` + `index-of` (`streq`), and char comparison goes through
   `index-of` membership — never `=`. This is the single most surprising wart:
   `[= "ab" "ab"]` is `false`. **Fix:** make `=` structural for strings (and
   ideally collections), with identity available as a separate `same?` if
   needed. _break? yes (subtle) · effort M_

2. **`{…}` string interpolation collides with set/map literals.** You cannot
   write a literal `{` in a string: `"#{IO}"` becomes `#IO`, and `"#{}"` errors
   with "unreachable code". Every test that embeds Loon source with a `#{…}`
   effect set or `{…}` map must assemble braces from `ch-ob`/`ch-cb`. **Fix:**
   pick an interpolation sigil that doesn't overload braces — e.g. `\(expr)`
   (Swift-style) or `${…}` — or require `{{`/`\{` to escape. _break? yes ·
   effort S–M_

3. **String indexing is O(index).** `char-at`/`substring` re-scan from the
   start, so naive scanning is O(n²); the reader works around this by splitting
   the whole source into a char vector once. **Fix:** O(1) indexable strings
   (or a documented cursor/bytes API). _break? no · effort M_

4. **Built-in vs declared effects are inconsistent.** `samples/effects.oo` uses
   `IO`/`Fail` with no `[effect …]` declaration (they're implicit), but user
   effects must be declared. **Fix:** either declare everything (with a `prelude`
   of standard effects) or document a fixed built-in set. _break? maybe · S_

4b. **Builtins shadow user `fn` definitions.** Defining `[fn sum …]` does *not*
   override the builtin `sum` — `[sum xs]` still calls the builtin (discovered
   when a self-hosted test silently computed against the builtin instead of the
   user's recursive `sum`). User definitions should win, or at least collide
   loudly. **Fix:** user `fn` (and `let`) bindings shadow builtins; warn on
   shadow. _break? yes (subtle) · effort S_

4c. **`match` does not discriminate constructors across different types.** A
   pattern `[LCtx a b c d]` happily matched a `[VB s i]` value (a different
   type, even different arity) — match appears to assume the scrutinee has the
   pattern's type and only discriminates *within* a type. This silently binds
   garbage when a heterogeneous collection holds values of several types.
   **Fix:** match should check the constructor's owning type/tag, not just shape.
   _break? yes (catches latent bugs) · effort M_

5. **`let` is a body statement, not an expression.** `[let x v]` scopes to the
   rest of the enclosing body — clean in sequences, but `[let …]` can't be used
   as a sub-expression (the lowerer/inferrer special-case it). **Consider:** a
   Clojure-style expression form `[let [x v …] body]` for composability, or keep
   the statement form but make it a true expression returning its body value.
   _break? depends · effort M_

## B. Missing builtins / stdlib

6. **No `string → number` parsing.** The reader deliberately keeps numeric
   lexemes as strings, but then *everyone* downstream needs to parse them; the
   self-hosted VM had to hand-roll `parse-int` (digit fold via `index-of`).
   **Add:** `int`/`float`/`parse-int` (and confirm `str` covers number→string).
   _break? no · effort S_

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

10. **One-shot continuations block stateful handlers.** The expander wanted a
    `Gensym` effect but couldn't, because the one-shot VM can't keep handler
    state across resumes (a pure handler hands out the same id every time) — it
    had to use call-site spans instead. **Fix:** multi-shot / resumable
    continuations (decision D3) in the self-hosted VM, which also unlocks
    generators, async, and a real `State`/`Ref` effect (see #11). _break? no ·
    effort L_

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
