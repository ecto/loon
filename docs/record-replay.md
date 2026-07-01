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
doubles as a structured log of the run. Pure operations (`parse-json`,
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
holds still while you look at it.

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
error: replay diverged at step 2: trace recorded IO.println but the program performed IO.millis
fix: the program and the trace no longer agree; re-record with: loon run --record crash.oo prog.oo
```

Running past the end of the trace is diagnosed the same way, and a replay
that finishes with unused entries left over prints a warning — both usually
mean the program changed since the recording.

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
