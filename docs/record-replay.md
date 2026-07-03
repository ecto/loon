# Record/Replay Debugging

Loon tracks every effect a program performs. That has a security payoff
(see [capabilities.md](capabilities.md)) — and a debugging payoff: if you
record the *results* of a program's nondeterministic effects, you can replay
the exact same execution later. Same trace, same run, same crash. Every time.

## The problem

Some bugs only happen on some runs: a request races a timeout, a clock lands
on an unlucky value, a file changes between reads. You see the crash once in
CI and never again on your machine.

In Loon, all of that nondeterminism flows through effects — `IO.millis`,
`IO.read-file`, `IO.uuid`, `Env.get`, `Net.get`. There is no ambient
randomness hiding in library code. So the set of things you need to capture
to make a run reproducible is exactly the set of effect results.

## Recording a run

```bash
loon run samples/replay-demo.oo --record crash.oo
```

This runs the program normally, but every nondeterministic effect result is
appended to `crash.oo` in Loon data format — a vector of maps:

```loon
#[{:effect "IO" :op "millis" :args #[] :result 1751338712345}
  {:effect "IO" :op "println" :args #["processing batch (roll 0)"] :result :unit}
  {:effect "IO" :op "println" :args #["corrupt record encountered!"] :result :unit}]
```

Clock reads, uuids, file reads, env lookups, and network calls are recorded.
Log writes (`println`) are recorded too, purely for observability — the trace
doubles as a structured log of the run, and on replay they re-execute live
rather than being matched against the trace. Pure operations (`parse-json`,
`blake3`) are not recorded; they are deterministic given their arguments.
Effects handled by an in-language `handle` never reach the trace at all —
they are already deterministic.

Entries are flushed to disk as they happen, so **the trace survives a crash
mid-run**. That is the point: the runs you most want on tape are the ones
that die.

## Replaying it

```bash
loon replay crash.oo samples/replay-demo.oo
```

The program runs again, but instead of touching the outside world, each
effect operation is fed the recorded result in order. The clock reads the
recorded milliseconds. The file read returns the recorded contents — even if
the file is gone. The execution is identical to the recorded run, including
reproducing the crash at the same step, with the same output and the same
error report. Run it under a debugger, add prints, replay again: the bug
holds still while you look at it. Log writes are never matched against the
trace, so sprinkling `println`s through the program (or deleting them) does
not invalidate the recording — the nondeterministic results are still fed
back in order around them.

## Walkthrough: pinning down a flaky crash

[`samples/replay-demo.oo`](../samples/replay-demo.oo) crashes on roughly one
run in five, depending on the wall clock:

```loon
[fn validate-batch [roll]
  [if [= roll 0]
    [do
      [println "corrupt record encountered!"]
      [assert-eq roll -1]]
    [println "batch \(roll) ok"]]]

[fn main []
  [let roll [% [IO.millis] 5]]
  [println "processing batch (roll \(roll))"]
  [validate-batch roll]
  [println "all batches processed"]]
```

Run it with recording until you catch a crash (the trace is rewritten on
each run):

```bash
$ loon run samples/replay-demo.oo --record crash.oo
processing batch (roll 3)
batch 3 ok
all batches processed

$ loon run samples/replay-demo.oo --record crash.oo
processing batch (roll 0)
corrupt record encountered!
error: assertion failed: 0 != -1
note: crash trace saved to crash.oo — reproduce it with: loon replay crash.oo samples/replay-demo.oo
```

Now the crash is deterministic:

```bash
$ loon replay crash.oo samples/replay-demo.oo
processing batch (roll 0)
corrupt record encountered!
error: assertion failed: 0 != -1
   ┌─ samples/replay-demo.oo:21:7
   │
21 │       [assert-eq roll -1]]
   │       ^^^^^^^^^^^^^^^^^^^
```

Every replay of `crash.oo` produces exactly this run — same roll, same
output, same failing assertion at the same source span.

## Divergence

A trace is only valid for the program (and effect order) it was recorded
from. If the program changes and performs a different operation than the
trace recorded, replay stops with a diagnostic instead of feeding it a wrong
value:

```
error: replay diverged at step 2: trace recorded IO.millis but the program performed IO.uuid
fix: the program and the trace no longer agree; re-record with: loon run --record crash.oo prog.oo
```

The same operation with different *arguments* — say, a changed file path —
is also a divergence, not a silent replay of the stale recorded result:

```
error: replay diverged at step 0: IO.read-file was recorded with args #["a.txt"] but the program passed #["b.txt"]
```

Running past the end of the trace is diagnosed the same way, and a replay
that finishes with unused entries left over prints a warning — both usually
mean the program changed since the recording. Log writes are exempt from
all of this: `println` entries are observability-only and never
order-checked, so added or removed prints neither diverge nor count as
leftovers.

## `loon verify` — the fix oracle

Replay makes a crash reproducible; `loon verify` turns that reproducer into
a *verifier*. After you (or an agent) change the program, one command answers
the only question that matters: is the recorded crash actually gone?

```bash
loon verify crash.oo samples/replay-demo.oo
```

It replays the trace against the program and classifies the outcome into a
three-state contract, each with its own exit code:

| Verdict | Exit | Meaning |
|---|---|---|
| `FIXED` | 0 | The program consumed the trace compatibly and the recorded crash did not reproduce. The bug is gone — under the exact world that used to kill it. |
| `REPRODUCED` | 10 | The program crashed with the same error class at the same step as the recording. The bug still exists. |
| `DIVERGED` | 11 | The program requested a different effect op (or the same op with different args) before reaching the crash point, or crashed with a *different* error. The change altered behavior beyond the bug — the verdict names the step and the expected vs requested op. |

Bad input (missing trace, unparseable program, wrong usage) exits 1, never a
verdict.

"Consumed compatibly" is defined carefully: a fixed program may legitimately
stop performing effects before the end of the trace (a guard now short-circuits)
or keep going *past* the end of it (the recording stopped at the crash; the fix
sails on to the next batch). Both are `FIXED` — the guarantee is that the whole
recorded history was replayed without a mismatch and without the recorded
crash. What the program does beyond the recorded history is explicitly not
verified, and the verdict says so.

### The recorded outcome

To make the comparison exact, `loon run --record` now appends the run's
outcome to the trace as one extra map:

```loon
{:outcome "crash" :error-class "assert-failed"
 :error "assertion failed: 0 != -1" :steps 3}
```

`:steps` counts the nondeterministic (non-log) operations recorded —
the crash's position on the tape. Error classes are stable machine-readable
names: `assert-failed`, `divide-by-zero`, `unhandled-effect`, `not-callable`,
`stack-overflow`, `trap`.

Traces recorded by older versions have no outcome map. They still replay and
verify, but without ground truth the verdicts degrade: `COMPLETED` (exit 0)
instead of `FIXED`, `CRASHED` (exit 10, with the crash class and step)
instead of `REPRODUCED` — and the detail text states which guarantee cannot
be made and suggests re-recording. `DIVERGED` needs no ground truth and is
unchanged.

### `--json` for harnesses

```bash
loon verify crash.oo prog.oo --json
```

emits a single JSON object on the last line of stdout (replayed log writes
still print above it):

```json
{
  "verdict": "fixed",             // "fixed" | "reproduced" | "diverged"
                                  //   | "completed" | "crashed" (old traces)
  "exit_code": 0,                 // 0 | 10 | 11
  "step": 1,                      // most relevant step index, or null
  "detail": "program completed without the recorded crash (…)",
  "trace_ops_consumed": 1,        // nondeterministic ops fed back
  "trace_ops_total": 1,           // nondeterministic ops in the trace
  "recorded_outcome": {           // null for old traces
    "status": "crash",            // "crash" | "ok"
    "error_class": "divide-by-zero",
    "error": "division by zero",
    "steps": 1
  }
}
```

### The agent workflow

This is [Principle 3 of agent-first design](agent-first.md): the language as
an oracle for its own output. The loop:

1. **Record** the failure: `loon run prog.oo --record crash.oo` (rerun until
   the flaky crash lands on tape — the trace survives the crash).
2. **Confirm** the reproducer: `loon verify crash.oo prog.oo` → `REPRODUCED`
   (exit 10). You now hold a bug that cannot escape.
3. **Fix** the program.
4. **Verify**: `loon verify crash.oo prog.oo` → `FIXED` (exit 0) means the
   crash is gone under the exact recorded world — knowledge, not belief.
   `DIVERGED` (exit 11) means the change did more than fix the bug: the
   verdict names the first step where behavior differs.
5. **Ship the proof**: attach the trace and the verify output to the PR. Any
   reviewer (or CI) can re-run the same command and get the same verdict.

## Scope and limitations

- Record/replay runs on the default backend (the EIR register VM).
  Combining `--record` with `--legacy`, `--wasm`, or `--native` is an error.
- Replay does not re-execute external writes: `IO.write-file` and network
  sends are skipped and their recorded results returned, so replaying a
  crash cannot corrupt files that the original run touched. Log writes are
  the exception — they re-print, so the replayed output matches the run.
- The trace records effect *results*, not a full heap snapshot. Programs
  whose nondeterminism comes from outside the effect system (there should
  be none) are not captured.
