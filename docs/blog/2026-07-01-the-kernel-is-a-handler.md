# The Kernel Is Just the Outermost Handler

*July 2026*

For a couple years now I've been telling people that algebraic effects are "just" a very principled version of exceptions that can resume. True, boring, and it never once made anyone's eyes light up. Then last week I was staring at a syscall table trying to explain to myself why `chroot` and `strace` and sandboxes all felt like the same shape wearing different uniforms — and it finally clicked.

A syscall is an effect. And if a syscall is an effect, then the kernel is just the outermost handler.

That's the whole post. Everything below is me failing to stop pulling on that thread.

## The move

In a normal operating system, a syscall is a trap. Your program hits an instruction, control disappears into privileged code you don't get to see, and something happens to the real world on your behalf. It's opaque by design. That opacity is exactly why sandboxing, tracing, and replay are each their own separate machinery — you can't get between the program and the kernel, so you build a `ptrace`, or a seccomp filter, or a whole VM, one bespoke contraption per feature.

In Loon, a syscall isn't a trap. It's a `perform`. Reading a file is `[Fs.read-file p]` — an effect that floats up through the call stack until some handler catches it. And the kernel is just the handler you happen to wrap around `main`. Here it is. Not a diagram of it. The whole thing.

```
[fn kernel [thunk]
  [handle [thunk]
    [Fs.read-file p]    [resume [IO.read-file p]]
    [Fs.write-file p c] [resume [IO.write-file p c]]
    [Clock.now]         [resume [IO.now]]
    [Rand.int n]        [resume [imod [IO.millis] n]]
    [Env.get k]         [resume [Env.get k]]]]
```

That's it. Each effect maps to a real IO primitive, resumes the program with the result, and the program has no idea any of that happened. There's no privilege here — just the handler that happens to be furthest out. Run a thunk inside `kernel` and it talks to the actual disk and the actual clock. Run it inside a different handler and it talks to whatever you want.

That's the click. Once the kernel is a handler, every classic OS feature stops being its own subsystem and becomes the same one move: *wrap the program in a different handler.*

## Chroot is a library now

Watch. Here's `chroot`. Not a privileged operation, not a mount namespace — a function.

```
[fn sandboxed [thunk root]
  [handle [thunk]
    [Fs.read-file p]    [resume [Fs.read-file [str root p]]]
    [Fs.write-file p c] [resume [Fs.write-file [str root p] c]]]]
```

It catches `Fs` effects, rewrites the path, and re-performs them — which sends them up to the *next* handler out, eventually the real kernel. The program asked for `/task`, it got `/sandbox/task`, and nothing in the program knows. Four lines and no root.

Same move for a reference monitor. Here's a gate for an untrusted agent, where a denied read doesn't crash — it resumes with an `EACCES` sentinel, because a handler decides what "denied" even means.

```
[fn gated [thunk policy]
  [handle [thunk]
    [Fs.read-file p]
      [if [policy {:op :read :path p}]
        [resume [Fs.read-file p]]
        [resume [str "EACCES: " p]]]]]
```

No trap, no crash, no exception to catch. A denial just resumes with a string, exactly like a real kernel handing back an error code. The agent gets an answer. The answer is no.

strace is a handler that logs and forwards. Record/replay is a handler that writes results to a tape, then a second handler that reads them back. Deterministic simulation is a handler where `Clock.now` returns whatever you say and `Rand.int` reads from a seed. None of these need kernel support. They're all the same wrapping, differing only in what the clause does before it resumes.

The whole "OS" is nine small Loon files — pure Loon, running today on the EIR VM. No new runtime primitives. I keep waiting for the catch and it hasn't shown up.

## What you can do when you stack them

The one I can't stop showing people is agent containment. Take an untrusted agent, let it do honest work, then let it try to steal credentials and phone home. Wrap it in `gated` with a `/workspace/*` policy, wrap *that* in a tracer, wrap *that* in a seeded simulator. Then run it.

```
agent finished. what it got away with:
  {:answer 42 :stole EACCES: /etc/credentials :exfil-result :denied}
answer file: '42 (compute the answer)'
exfil file:  '<never written>'
audit log (every attempt + what it returned):
  {:op :read :path /workspace/task :got compute the answer}
  {:op :write :path /workspace/answer :bytes 23 :got ()}
  {:op :read :path /etc/credentials :got EACCES: /etc/credentials}
  {:op :write :path /evil/exfil :bytes 24 :got :denied}
rerun, same seed: AGENT RUN REPRODUCED EXACTLY
```

The credential read got a sentinel. The exfil write went nowhere. Every attempt is in the audit log — including the two crimes — because the monitor sees *every* effect, not the ones the agent chose to declare. And because the whole thing ran under a seeded handler, it replays byte-for-byte. That last line is the one that makes me trust it. You can't reproduce a container.

Record/replay is the same trick pointed at time. Record with the real kernel underneath, then replay with *no kernel at all* — the tape answers every effect, so it touches nothing real:

```
recording (real IO, real clock):
  live:   read 'hello from the past' at t=1782950917104 (roll: 104)
replaying from tape (no kernel, no real IO):
  replay: read 'hello from the past' at t=1782950917104 (roll: 104)
REPLAY IDENTICAL
```

## The part where I admit it wasn't free

I'd love to say all nine files fell out clean. They did not. Building this dragged two genuine bugs out of the VM, and both were the kind that only surface when handlers get nested more than one deep — which, well, that's the entire premise here.

The first: handler forwarding infinite-looped. A clause that re-performed its own effect — exactly what `sandboxed` does when it rewrites a path and calls `Fs.read-file` again — was recursing back into *itself* instead of forwarding to the next handler out. So `chroot` was an infinite loop. Pegged a core and just sat there. I'd built a handler that could only ever forward to itself, which is a very elaborate way to write `while true`. The fix was deep-handler semantics: a re-performed effect resumes searching from the handler *outside* the current one. It's the difference between "who handles this effect" and "who handles this effect *next*." Small change. Load-bearing.

The second was worse, because it didn't hang. It just lied. `try` was compiling its on-fail closure with no free-variable capture — so the supervision retry pattern, the thing restarting the chaos worker below, was silently reaching for variables that weren't there. No crash. Just wrong. An infinite loop announces itself. A closure that captures nothing gives you slightly wrong answers, forever, until you happen to build the one program that notices. The chaos demo is a supervised worker under seeded fault injection, and it's the reason I caught it:

```
chaos run, seed 3 (a stormy one):
  [supervisor] child failed: chaos: read error on /config — restarting
  [supervisor] child failed: chaos: write error on /result — restarting
  ...
  outcome: ok (v1)
same seed again (same storm, same restarts): CHAOS REPRODUCED EXACTLY
```

Two bugs, both small fixes, both now pinned with regression tests. 566+ tests pass. The legacy tree-walking interpreter still gets several of these nested-effect cases wrong, so as of today the EIR VM is the reference — the old interpreter is the one that's lying.

## Where this goes

I don't have a grand roadmap. I have a strong suspicion that "the kernel is a handler" is one of those ideas that's obvious in hindsight and load-bearing in practice, and I want to keep pushing on it until it breaks. Scheduling as a handler. Capabilities as handlers you can't unwrap. Maybe a real process boundary that's nothing but a handler you're not allowed to escape.

A permission you can't enforce is a wish. A permission that *is* the handler stack is a fact.

If you want to see the whole thing, it's nine files in `os/`. Read `kernel.oo` first — it's the entire trick, and it's shorter than this paragraph. Then go wrap a program in a handler it doesn't know about and watch what you can do to it.