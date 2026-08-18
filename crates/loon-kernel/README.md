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

The host toolchain is not in this crate's build graph. `build.rs` shells out
to `loon image`, which compiles `boot/init.oo` to a boot image; the kernel
embeds that image and interprets it. Everything upstream of EIR — parser,
checker, ownership, lowering — stays on the host, where it belongs.

## Why the output has to match

`make check` diffs the machine against the host. That diff is the point of
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

## Known limits

- **Cooperative only.** No timer interrupt yet, so a pure loop owns the
  machine. Preemption is the next real milestone.
- **Single hart.** The allocator's "lock" is a bare cell because nothing
  races with it. SMP needs a real one.
- **Partial builtin set.** Intrinsics the runtime lacks raise a loud error
  naming the builtin; they never silently return `()`.
- **Slow.** Roughly 0.9 µs per interpreted op — the interpreter allocates a
  register file per call and an operand vector per op, on a first-fit
  allocator that costs ~0.5 µs per allocation. Nothing here is tuned yet.
