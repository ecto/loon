# Subject-last ergonomics for the vcad stdlib

Date: 2026-07-18. Status: implemented, design open for review.

The vcad stdlib (vcad `lib/src/lib.loon`) is subject-last so pipes read
naturally: `[pipe body [difference tool]]`. But a *direct* call
`[difference A B]` computes B − A — reversed from every other CAD system —
and yields a valid-but-wrong solid with no error. Two related traps:
`[30 20 -1]` is application (→ "not callable: 30"), and there was no way to
discover a symbol's arity/arg names without reading lib.loon.

## Problem 1 decision: lint, not new calling conventions

Options considered:

- **(a) keyword/record args** (`[difference {:from body :cut tool}]`) —
  order-proof, but a second calling convention that breaks pipe threading
  (pipe appends the subject positionally), breaks every existing `.loon`
  file if made mandatory, and is optional otherwise (an optional safety
  doesn't catch the user who doesn't know about the trap). Rejected.
- **(c) subject-first alias family** (`cut`, `move`, …) — doesn't make the
  *existing* names any safer; doubles the vocabulary and splits the
  ecosystem into two dialects. Rejected as the primary fix (an alias can
  still be added later as sugar).
- **(b) lint on direct calls to subject-last builtins** — chosen, with a
  precision refinement:
  - `difference` at full arity (2 solid args): always warn — the two
    operands are both solids, so wrong order is statically undetectable;
    the warning states the semantics ("`[difference A B]` computes B − A")
    and shows both correct spellings (swap, or pipe).
  - `translate`/`rotate`/`scale`/`mirror` at full arity: warn **only when
    the last argument is a numeric literal** — the subject slot holding a
    number means the args are definitely reversed. Correct direct calls
    stay silent.
  - `union`/`intersection`: no warning — commutative, order can't be wrong.

This makes the mistake loud without regressing pipes (pipe-stage calls are
under-arity and never match), without breaking any existing source
(warnings, not errors), and with zero false positives for transforms.

Implemented as `vcad_loon::lint_vcad(source) -> Vec<VcadWarning>` (message +
1-based user line). Consumers (Typst plugin, CLI) call it alongside
`eval_vcad` and surface warnings however they render diagnostics. It lives
in vcad-loon rather than loon-lang because subject-last-ness is vcad stdlib
knowledge, not a language property. If more stdlib fns need it, the table
could move into lib.loon metadata later.

## Problem 2: the vector trap error + spans across the FFI

- loon-lang: all "not callable" sites (`interp/mod.rs` call + pipe paths,
  `interp/machine.rs`, `interp/builtins.rs::apply_value`) now share
  `not_callable_msg`, which special-cases a numeric callee: "30 is a
  number, not a function. loon has no [x y z] vector syntax; write
  #[30 ...] for a vector, or pass components as separate arguments, e.g.
  [translate 30 20 -1 solid]".
- vcad-loon: errors no longer collapse to a bare `String`. New
  `VcadError { message, line: Option<usize> }` and
  `eval_vcad_diag` / (internal) span→line mapping that subtracts the
  prepended lib prefix; spans inside the bundled lib map to `line: None`.
  The existing `eval_vcad`/`eval_vcad_to_value` `String` APIs are kept
  backward-compatible and now prefix "line N: " via `VcadError`'s Display.
- The multi-root `collect_top_level_values` rewrite now copies inter-form
  text verbatim and wraps value forms in place (no inserted newlines), so
  line numbers survive the rewrite.

## Problem 3: discoverability

- `signature` builtin in loon-lang: `[signature translate]` →
  `"[translate x y z s]"` (multi-clause fns join with `|`, rest params show
  as `& name`, builtins report `"[name ...] (builtin)"`). Registered in the
  checker as `∀a. a → Str`.
- No new `[vec x y z]` helper: `#[x y z]` already exists and the improved
  not-callable message now teaches it at exactly the moment of the mistake.
  A parse-time diagnostic for `[number ...]` was skipped — the runtime error
  now fires with a span and the guided message, and parse-level rejection
  would outlaw macro-generated forms that are legal today.

## Tests

- loon-lang `tests/interp_tests.rs`: number-callee message (tree-walker and
  VM), plain message preserved for non-numeric callees, `signature` on
  named/anonymous fns, builtins, and non-fns.
- vcad-loon `ergonomics_tests`: error carries user line (including through
  the multi-root rewrite), lint fires on direct `difference` and reversed
  transforms, stays silent on piped/correct calls, `[signature translate]`
  reads stdlib arg names.
