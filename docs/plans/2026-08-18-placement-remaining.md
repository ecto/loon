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

### The browser

`crates/loon-wasm` embeds the tree-walking interpreter, not the EIR VM, so
none of the above reaches a tab. The blocker is the DOM bridge (`lib.rs:46`),
which predates placement entirely. The placement code itself compiles for
`wasm32-unknown-unknown` today:

    cargo check -p loon-lang --target wasm32-unknown-unknown

Moving the wasm build onto the EIR VM is its own project. Once done, wgpu's
WebGPU backend and async device initialization are the remaining pieces.

### Reductions and atomics

Kernels are map-shaped: a work item writes at its own index. A reduction needs
workgroup-shared memory and a two-phase dispatch, and the disjointness
argument that makes parallel placement sound stops holding. Host-side
reduction after `Place.read` works today and is what the samples do.

### 64-bit on a GPU

WGSL core has no `f64` or 64-bit integer. Buffers of those types are narrowed
to 32-bit on the device, reported rather than hidden (`DType::gpu_ok`).

### Scatter kernels

`check::kernel` does not yet enforce that a `put` index is the work index. The
parallel executor catches a violation at runtime — a write outside the slice a
thread owns is an error naming the problem — but catching it at compile time
would be better.

### Numbers we do not have

No comparison against hand-written CUDA, HIP, or Metal. No RAJAPerf port. The
benchmark's CPU column is Loon's own typed executor, which is a fair floor but
not an optimized C baseline, and nothing in `BENCHMARKS.md` should be read as
one.
