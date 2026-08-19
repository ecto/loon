# Placement: what is done and what is not

*August 2026 — companion to `docs/blog/2026-08-18-placement-is-an-effect.md`*

Written down so the next person does not have to infer the boundary from the
code.

## Shipped

- **Buffers** (`eir::vm::Buffer`) — dense, unboxed numeric arrays, the
  representation that can leave the process.
- **The `kernel` form** — desugars to `fn` before anything else looks at it;
  `check::kernel` enforces the subset and names what it rejects.
- **The `Place` effect** — `run`, `read`, `pin`, `unpin`, `stats`. Unhandled,
  a program runs serially right here.
- **Placement modes** — `--place cpu | par | device | gpu`, all agreeing on the
  answer, plus `--place-stats`.
- **A typed kernel executor** (`eir::kernel_exec`) — the numeric subset over raw
  slices, and a parallel driver that carves *every* output buffer into
  per-thread slices with `split_at_mut`.
- **WGSL emission** (`eir::wgsl`) — validated by naga in CI on machines with no
  GPU.
- **Real GPU execution** (`eir::gpu`, feature `gpu`) — wgpu, with resident
  device buffers so the transfer accounting describes the hardware.
- **Record and replay** — a run on the GPU replays on a build with no GPU.
- **Handlers** (`os/place.oo`) — trace, dry-run, counted, resident,
  resident-only.
- **Reductions** (`samples/place/reduce.oo`) — no new feature needed: a work
  item sums its own chunk into its own slot, which is still "write at your own
  index", and the partials are combined on the host.
- **Placed programs in a browser, including on the GPU** — `crates/loon-wasm`
  exports `eval_placed`; `web/public/place.html` runs cpu, par, device, and
  **real WebGPU** in a tab, all four agreeing on the answer. Because Loon's VM
  is synchronous and WebGPU is not, the VM runs in a worker and blocks on
  `Atomics.wait` while the main thread drives the device.
- **`eir::device::Device`** — the six operations a placement backend provides.
  wgpu implements it natively; `loon-wasm` implements it by proxying to
  JavaScript. The VM does not know which it has.

## Decided, and deliberately restrictive

These are not gaps. They are the rules the design rests on, enforced.

- **A kernel writes at its own index and nowhere else.** Rejected at compile
  time (E0602), naming the offending index. Reading anywhere is fine — gather is
  safe, scatter is not. This is what lets the parallel executor hand each thread
  a slice and a GPU run every work item at once.
- **No 64-bit on a GPU.** WGSL core has no `f64` or 64-bit integer, so such a
  launch is refused with a message naming the type and the alternative rather
  than narrowed silently. Handing back a precision the program never asked for,
  and cannot detect, is the failure this whole design exists to avoid.
- **Kernels cannot allocate, close over, or perform effects.** The restriction
  is the safety argument: the unsafe program is not rejected, it is unwriteable.

## Not done

### Reaching a GPU without cross-origin isolation

Two paths now exist, and the second one no longer needs isolation.

**The worker path**, which is what `web/public/place.html` uses today: the VM
runs in a Web Worker and blocks on `Atomics.wait`, which needs
`SharedArrayBuffer`, which needs COOP/COEP headers (`vercel.json` sets them).
Verified on a real GPU.

**The parking path**, which needs none of that. A handler that hands `resume`
to `Host.park` and returns unwinds the computation, and the page finishes it
when the bytes arrive. `eir::vm::Session` keeps the VM alive between steps, and
`place_start` / `place_resume` expose it to JavaScript. Confirmed in a browser
with `crossOriginIsolated === false`:

    start:  done=false                              ← parked at Place.read
    resume: done=true out="read #[2 4 6]" value=12  ← finished with host data

An earlier version of this document said this would need "an asynchronous
effect path in the VM". It did not: escaping continuations already were one.

What is left is only to point the demo page at it. That means writing the
sample with the deferring handler outermost — parking unwinds to the `handle`,
so anything after it runs immediately with the placeholder (see
`os/demo-park.oo`, and the test named for it). Uploads and dispatches need
nothing, because `writeBuffer` and `submit` are already synchronous; only the
readback ever has to wait.
### The DOM exports

`eval_ui` and `invoke_callback` remain on the legacy tree-walking interpreter.
Its bridge is written against `Value`/`InterpError` rather than the EIR's
`Val`/`VmResult`. `eval_program` and `eval_with_output` were left with them
rather than regressing documented pages; `eval_placed` was added alongside.

The deeper blocker is the one below, found while looking into this.

### The mutators, which need a decision rather than an implementation

`set!` and `push!` are documented — `web/src/pages/guide/collections.loon`,
`guide/ownership.loon`, `ref/builtins.loon`, `DESIGN.md` — and **neither exists
on the EIR VM**, which is the default backend. On the interpreter they behave
like this:

    [let mut v #[1 2 3]]
    [push! v 4]
    [println v]        ; #[1 2 3]  — unchanged
    [println [push! v 9]]  ; #[1 2 3 9]

So `push!` does not mutate. It returns a new vector, and the `!` promises
something it does not do. The guide's own example asserts otherwise:

    [let items [mut #[]]]
    [push! items 1]
    [push! items 2]
    [println items]  ; #[1 2]   ← documented

That prints `#[]` on the interpreter and fails to type check on the EIR VM. The
documented behaviour is currently true of no backend.

This was not implemented on the EIR VM as part of the placement work because
the right fix is a language decision, not a port:

1. **Make `!` mean mutation.** `push!` writes through the binding, matching the
   name, the guide, and what `put` already does for buffers. The ownership pass
   already classifies both as mutable borrows, so the analysis is in place. The
   question is what happens to a closure that captured the old value.
2. **Make `!` mean "returns a changed copy"** and fix the guide and the name.
   Smaller change, but then `set!` and `push!` do not agree with each other,
   since `set!` really does rebind.

Either way both need to exist on the default backend. Implementing the current
interpreter behaviour verbatim would spread a naming problem to a second
backend, so it is left for whoever decides which of the two Loon means.

### Atomics

Anything genuinely needing them — a histogram, a scatter-add — is out, and for
the same reason scatter is: two work items reaching the same element is exactly
what the disjointness rule forbids. Supporting them would mean a second kind of
kernel with a different safety argument, not a relaxation of this one.

### Numbers we do not have

No comparison against hand-written CUDA, HIP, or Metal. No RAJAPerf port. The
benchmark's CPU column is Loon's own typed executor, which is a fair floor but
not an optimized C baseline, and nothing in `BENCHMARKS.md` should be read as
one.
