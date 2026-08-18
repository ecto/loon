# loon-kernel — Loon as a unikernel

A RISC-V machine whose kernel is a Loon program. There is no userspace, no
syscall boundary, and no OS underneath: `boot/init.oo` performs effects, and
the outermost handler is a UART driver rather than a call into Linux.

```bash
brew install qemu
rustup target add riscv64gc-unknown-none-elf

make run      # boot it
make host     # run the same program on the host
make check    # boot it and diff the two
```

## What is here

| | |
|---|---|
| `src/main.rs` | entry, `.bss` clear, stack, the `Host` impl that is the machine |
| `src/uart.rs` | NS16550a console driver |
| `src/mmio.rs` | the one place that touches device registers |
| `src/heap.rs` | first-fit free-list allocator over RAM above the image |
| `src/sbi.rs` | the slice of SBI we need (power off) |
| `src/eir/` | boot-image decoder and the EIR interpreter |
| `boot/init.oo` | the init program — ordinary Loon |
| `boot/mandel.oo` | a Mandelbrot set, because a kernel that boots should get to do one gratuitous thing |

The host toolchain is not in this crate's build graph. `build.rs` shells out
to `loon image`, which compiles `boot/init.oo` to a boot image; the kernel
embeds that image and interprets it. Everything upstream of EIR — parser,
checker, ownership, lowering — stays on the host, where it belongs.

## Why the output has to match

`make check` diffs the machine against the host — init *and* the fractal,
which doubles as the float path's parity check. That diff is the point of
the exercise: the same program, the same effects, two entirely different
bottom halves. If they ever disagree, one of the two runtimes is wrong about
what Loon means, and a language whose semantics depend on where it runs is
not the language we are trying to build.

The interpreter therefore mirrors the host VM's structure rather than
reimplementing it freely — same frame stack, same handler stack keyed by
prompt depth, same continuation capture on `perform`. Deep-handler semantics
are load-bearing: a clause that re-performs its own effect must forward
outward, which only works if capturing moves every handler at or above the
prompt into the continuation. `boot/init.oo` exercises forwarding, aborting
(a clause that never resumes) and non-tail resume for exactly this reason.

## Benchmarking

```bash
./bench.sh 7     # boot 7 times, report the best result per benchmark
```

`boot/bench.oo` (calls and sequences) and `boot/loop.oo` (tail self-recursion,
which lowers to a jump and so never pushes a frame) run before init and report
ops, ns/op and allocation count. Op counts come from the VM's own dispatch
counter and allocation counts from the global allocator, so both are exact and
identical run to run. **Wall-clock is not:** it is measured inside an emulator
on a shared machine and only ever biased upward, which is why `bench.sh`
reports a minimum over several runs. Treat a timing difference under ~30% as
noise unless you can reproduce it by interleaving two builds.

## Performance notes

Two things were measured properly, and one of them was a surprise.

**`opt-level` dominates everything.** The crate was scaffolded with
`opt-level = "z"`, which suppresses the inlining a dispatch loop depends on.
Switching to `3` was worth **3.3x** on `bench` and 1.5x on `loop`, measured
back to back on the same machine. Nothing else came close.

**Allocation was never the bottleneck.** Pooling register files and keeping
operands off the heap took the `bench` workload from 89,858 allocations to 47,
and bought *no measurable time* — interleaved A/B runs put the two builds
inside each other's noise. The changes are kept because a kernel that
interprets in near-constant memory is worth having on its own terms: no
allocator pressure, no fragmentation over long uptimes, no dependence on a
heap that has no OOM killer behind it. They are not a speedup, and an earlier
claim that they would be was wrong.

**A slab allocator is therefore not worth building.** There is nothing left
for it to allocate, and even at 90k allocations the existing first-fit list
was not costing measurable time.

What remains is roughly 500 ns/op against ~7 ns/op for a minimal native
dispatch loop under the same emulator. That gap is real and unexplained;
chasing it needs an idle machine and a profiler, not more guessing.

## Known limits

- **Cooperative only.** No timer interrupt yet, so a pure loop owns the
  machine. Preemption is the next real milestone.
- **Single hart.** The allocator's "lock" is a bare cell because nothing
  races with it. SMP needs a real one.
- **Partial builtin set.** Intrinsics the runtime lacks raise a loud error
  naming the builtin; they never silently return `()`.
- **Slow.** Roughly 500 ns per interpreted op under emulation, against ~7 ns
  for a minimal native dispatch loop on the same emulator. See the
  performance notes above for what that is and is not.
