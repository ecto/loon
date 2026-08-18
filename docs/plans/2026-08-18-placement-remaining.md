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
- **Placement modes** — `--place cpu | par | device | gpu`, all agreeing on
  the answer.
- **A typed kernel executor** (`eir::kernel_exec`) — the numeric subset over
  raw slices, plus a parallel driver splitting the index range with
  `split_at_mut`.
- **WGSL emission** (`eir::wgsl`) — validated by naga in CI on machines with
  no GPU.
- **Real GPU execution** (`eir::gpu`, feature `gpu`) — wgpu, with resident
  device buffers so the transfer accounting describes the hardware.
- **Record and replay** — a run on the GPU replays on a build with no GPU.
- **Handlers** (`os/place.oo`) — trace, dry-run, counted, resident,
  resident-only.

## Not done

### WebGPU from a browser

Placed programs *do* run in a browser: `crates/loon-wasm` exports `eval_placed`,
which runs on the EIR VM, and `web/public/place.html` exercises cpu, par, and
device placement in a tab with transfer accounting. The residency handler works
there.

What is missing is the GPU behind it. wgpu's WebGPU backend needs asynchronous
device initialization (`request_adapter`/`request_device` return futures that a
browser resolves on its event loop), and `eir::gpu` uses `pollster::block_on`,
which cannot block on wasm. `--place gpu` in a browser refuses and says so.

The DOM-driving exports (`eval_ui`, `invoke_callback`) remain on the legacy
interpreter. The bridge is written against `Value`/`InterpError` rather than the
EIR's `Val`/`VmResult`, and the guide's examples use builtins such as `push!`
that the EIR VM does not implement — so `eval_program` and `eval_with_output`
were left alone rather than regressing documented pages.

### Atomics

Reductions are done — see `samples/place/reduce.oo`. They needed no new feature:
each work item sums its own chunk into its own slot of a partials buffer, which
is still "write at your own index", and the handful of partials is combined on
the host. Anything genuinely needing atomics (a histogram, a scatter-add) is
still out, and that is the same restriction as the scatter rule below.

### 64-bit on a GPU

WGSL core has no `f64` or 64-bit integer, so a launch with such a buffer is
refused with a message naming the type and the alternative. It is not narrowed
silently — handing back a precision the program never asked for, and cannot
detect, is the failure this design exists to avoid.

### Scatter kernels

Rejected at compile time now (E0602), naming the offending index. Reading
anywhere is still allowed: gather is safe, scatter is not.

### Numbers we do not have

No comparison against hand-written CUDA, HIP, or Metal. No RAJAPerf port. The
benchmark's CPU column is Loon's own typed executor, which is a fair floor but
not an optimized C baseline, and nothing in `BENCHMARKS.md` should be read as
one.
