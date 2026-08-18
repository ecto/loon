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
- **Placed programs in a browser** — `crates/loon-wasm` exports `eval_placed`;
  `web/public/place.html` runs cpu, par, and device placement in a tab with
  transfer accounting, and the residency handler works there.

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

### WebGPU behind the browser

Programs run in a tab; the GPU behind them does not. wgpu's WebGPU backend needs
asynchronous device initialization — `request_adapter` and `request_device`
return futures a browser resolves on its event loop — and readback is only
available through `map_async`. `eir::gpu` blocks on both with `pollster`, which
cannot block on wasm. `--place gpu` in a browser refuses and says so.

Closing this needs one of two architectural changes, neither of them small and
neither of them really about placement:

1. An asynchronous effect path in the VM, so `Place.read` can suspend and
   resume when the browser delivers the mapped buffer. The VM's `builtin_effect`
   is synchronous throughout.
2. Running the VM in a Web Worker and blocking on `Atomics.wait` against a
   `SharedArrayBuffer` while the main thread performs the async work. Needs
   COOP/COEP headers and a message protocol.

### The DOM exports

`eval_ui` and `invoke_callback` remain on the legacy tree-walking interpreter.
Its bridge is written against `Value`/`InterpError` rather than the EIR's
`Val`/`VmResult`, and the guide's examples use builtins such as `push!` that the
EIR VM does not implement. `eval_program` and `eval_with_output` were left with
them rather than regressing documented pages; `eval_placed` was added alongside.

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
