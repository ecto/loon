# Capability Security

Loon's effect system is not just a typing discipline — it is the security
model. A Loon function cannot touch the filesystem, the network, the clock,
or the environment without that fact appearing in its type. There is no
ambient authority: no global `fs` module a transitive dependency can quietly
import, no hidden syscall surface. If code performs an effect, the effect is
visible, nameable, grantable — and deniable.

This document describes what is enforced today, how to audit it, and where
the model is still growing. Sibling feature: the same "all nondeterminism is
an effect" property powers [record/replay debugging](record-replay.md) —
capabilities and deterministic replay are two payoffs of one design.

## Effects make authority explicit

Every side effect in Loon goes through a declared effect operation:
`IO.read-file`, `Net.get`, `Env.get`, `Process.exec`. A function's effect
row is part of its signature:

```loon
[fn load-config [path] #{IO Fail}
  [IO.read-file path]]
```

The checker verifies the row. A function that claims to be pure (`#{}`) and
performs `IO` anyway fails `loon check` (the LSP surfaces the same
diagnostic as you type):

```
error: [E0401] function `quiet` performs undeclared effect `IO`
```

One caveat to state plainly: `loon run` does not currently gate execution
on these type errors — an unchecked program runs even if `loon check`
rejects it, so the check belongs in CI. Wiring the checker into `run` is
on the roadmap below.

This is the foundation the package-level story builds on: because effects
are typed, "what can this code do to the outside world?" is a static
question with a static answer.

## Per-dependency grants in `pkg.oo`

Dependencies are **pure by default**. A dep that needs authority must be
granted it explicitly in your manifest, per dependency:

```loon
{
  :name "my-app"
  :version "0.1.0"

  :deps {
    "github.com/cam/json" "^1.2"
    "github.com/cam/http" {:version "^1.0" :grant #["Net" "IO"]}
  }
}
```

`github.com/cam/json` gets no grants: it can parse, transform, and allocate,
but it cannot open a socket or read a file. `github.com/cam/http` is granted
exactly `Net` and `IO` — nothing else. Grants do not flow downward
automatically: a transitive dep does not inherit its parent's grants; the
chain of custody has to be declared at every level.

`loon add` takes grants on the command line, so the decision is made at the
moment you take on the dependency:

```bash
loon add github.com/cam/http --grant "Net,IO"
```

## Auditing the grant surface

`loon audit --capabilities` prints the grant table — every dependency and
the authority you have extended to it:

```
  Dependency Capabilities
  ──────────────────────────────────────────────────
  github.com/cam/json -> pure (no effects)
  github.com/cam/http -> Net, IO
```

`loon audit` (no flags) goes further: it walks the lockfile and checks
**transitive** grants — for each dependency's own dependencies, it verifies
the parent was actually granted the effects it passes down — plus cache
integrity (BLAKE3 content hashes) and lockfile staleness:

```
  Transitive Grants
    ✓ All transitive dependencies have required grants

  Cache Integrity
    ✓ 2/2 packages verified
```

## Walkthrough: catching a malicious transitive dep

Suppose you depend on `github.com/good/markdown`, a pure markdown renderer,
and a new release of it quietly adds a dependency on
`github.com/evil/telemetry`, which wants to exfiltrate your environment over
the network. In an ambient-authority ecosystem this is the classic supply
chain attack: nothing in *your* code changed, and the malicious code runs
with your process's full privileges.

In Loon the attack surfaces at three separate checkpoints:

1. **The manifest is the tell.** For `evil/telemetry` to perform `Net` at
   all, `good/markdown`'s own `pkg.oo` must declare
   `"github.com/evil/telemetry" {:grant #["Net" "Env"]}`. A markdown
   renderer requesting network and environment grants for a sub-dependency
   is visible in a one-line diff of its manifest.

2. **The audit flags it.** You granted `good/markdown` nothing (it is
   pure). `loon audit` walks the lockfile and reports the broken chain of
   custody:

   ```
   Transitive Grants
     ✗ github.com/evil/telemetry needs 'Net' (via github.com/good/markdown)
       'github.com/good/markdown' grants 'Net' to 'github.com/evil/telemetry',
       but 'github.com/good/markdown' itself is not granted 'Net' by the root manifest
     ✗ github.com/evil/telemetry needs 'Env' (via github.com/good/markdown)
   ```

   `loon audit` exits nonzero on violations, so this is a CI gate, not just
   a report.

3. **The runtime refuses.** If the code is executed anyway, performing an
   ungranted effect from dependency code is rejected at the perform site
   (today on the legacy interpreter — see the roadmap section):

   ```
   effect `Net` not granted to module `github.com/evil/telemetry` —
   add `:grant [Net]` in pkg.oo
   ```

The dependency never gets to choose its own authority. You do, in one file,
and the audit checks the whole tree against it.

## What is enforced today vs. roadmap

Honesty section. The current implementation:

- **Enforced by `loon check` (and the LSP):** effect rows on functions.
  Undeclared effects (`E0401`) and unhandled effects (`E0400`) are type
  errors with CI-gateable exit codes. `loon run` does not yet refuse to
  execute a program that fails these checks — running and checking are
  separate steps today.
- **Enforced by `loon audit`:** the transitive grant walk, content-hash
  verification of the package cache against the lockfile, and lockfile
  staleness — all CI-gateable exit codes.
- **Enforced at runtime:** the per-module grant check at the effect perform
  site currently runs on the legacy tree-walking interpreter
  (`loon run --legacy`). The default EIR VM does not yet enforce grants at
  runtime.

Roadmap (in the spirit of "violations are type errors, not sandbox kills"):

- **Grant checking in the type checker.** Since every module's effect row
  is inferred, checking dependency code against its manifest grant is a
  compile-time subsumption check — `dep's inferred effects ⊆ dep's grant`.
  This turns checkpoint 3 above from a runtime refusal into a build
  failure, uniformly across all backends.
- **Grant enforcement on the EIR VM**, closing the gap between the default
  backend and the legacy interpreter until the static check lands.
- **Gating `loon run` on the effect checker**, so an effect-row violation
  cannot execute at all — today that stop only happens when `loon check`
  is actually run.

The direction is fixed by the design: capability violations should be
caught by reading types, not by watching a sandbox die.

## One design, two features

The reason `loon replay` can reproduce any run from a trace of effect
results is the same reason `loon audit` can enumerate a dependency's entire
authority: **effects are the only door to the outside world.** Determinism
for debugging and least-authority for security are the same invariant viewed
from two sides. See [record-replay.md](record-replay.md).
