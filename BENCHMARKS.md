# Benchmarks

## Placement (v0.9.0)

Where a kernel runs is decided by a handler, not by the program. These numbers
say what that costs and what a residency policy is worth.

Harness: `cargo run -q --release -p loon-lang --features gpu --example bench_place`
Machine: Apple M4 Max (Metal, via wgpu). Numbers move a little run to run;
the transfer counts do not move at all.

### What a residency policy is worth

A chain of launches over one 4096-element buffer, on the GPU. The program is
identical in both columns — the only difference is whether `place/resident`
(nine lines, in `samples/place/lib.oo`) is wrapped around it.

| launches | no policy | place/resident | speedup |
|---------:|----------:|---------------:|--------:|
| 8 | 29.3 ms | 8.4 ms | 3.5x |
| 32 | 94.8 ms | 11.2 ms | 8.4x |
| 128 | 358.8 ms | 18.1 ms | 19.9x |

The gap grows with the chain because without a policy every launch uploads its
arguments, computes, and copies its results back; with one, the buffers stay
put and only the final `Place.read` moves anything. A recent Rust GPU-offload
paper measures the same gap at up to 400x between its convenient and explicit
interfaces, and closes it with `Preload`/`PreloadMut` annotations at every call
site plus a transfer-hoisting pass inside LLVM. Here it is a `handle` form.

### Transfers

Exact counts — these do not vary between runs.

| launches | no policy | place/resident | bytes saved |
|---------:|----------:|---------------:|------------:|
| 1 | 2 uploads | 2 uploads | 0 B |
| 4 | 8 uploads | 2 uploads | 96 KB |
| 16 | 32 uploads | 2 uploads | 480 KB |
| 64 | 128 uploads | 2 uploads | 2.0 MB |

### Kernel time: where it runs

The same kernel, the same program, four placements. The CPU columns go through
the typed executor in `eir::kernel_exec` — raw slices, no boxing — so this is a
fair floor rather than a straw man.

| elements | cpu | par | gpu |
|---------:|----:|----:|----:|
| 1,024 | 447 µs | 566 µs | 10.3 ms |
| 16,384 | 853 µs | 647 µs | 7.2 ms |
| 262,144 | 8.9 ms | 3.3 ms | 12.4 ms |
| 1,048,576 | 36.4 ms | 11.4 ms | 19.2 ms |

The interesting row is the last one: on this machine, **every core beats the
GPU** for this kernel at a million elements. An M4 Max has a lot of fast cores,
and a GPU launch pays submission and transfer before it computes anything. That
is not a disappointing result, it is the point — you find it out by changing one
word on the command line, because the program does not know where it runs.

The GPU only wins when the work per element is large enough to amortize getting
there, and where that crossover sits is a property of the machine, not of the
program. Which is a good argument for the decision living outside the program.

This is not a comparison against optimized C or a hand-written kernel. That
comparison is not attempted here and nothing above should be quoted as one.

### Launch overhead

| | ns per launch |
|---|---:|
| cpu | ~6,700 |
| gpu, buffers resident | ~92,000 |

Placement being an effect means every launch is an effect dispatch. That
dispatch is not what you are paying for: an effect operation costs roughly 3x a
function call (see the Effects section), which is nanoseconds, while a GPU
submission is tens of microseconds.

### What is not measured

- Any comparison against hand-written CUDA, HIP, or Metal. Not attempted.
- Reductions and atomics — outside the kernel subset for now.
- f64 on the GPU: WGSL core has no 64-bit scalar, so 64-bit buffers are
  computed in 32 bits and that narrowing is reported rather than hidden.

## Collection Benchmarks (v0.5.0)

100,000-element collections using `loop`/`recur` with persistent data structures (imbl).

Benchmark script: `samples/bench-collections.oo`

### Release mode (`cargo build --release`)

| Run | User | System | Total |
|-----|------|--------|-------|
| 1 | 0.70s | 0.03s | 1.137s |
| 2 | 0.69s | 0.03s | 0.730s |
| 3 | 0.71s | 0.02s | 0.743s |
| **Median** | **0.70s** | **0.03s** | **0.743s** |

Operations at 100K elements:
- Vec append (conj): 100K insertions
- Vec prepend (cons): 100K insertions
- Map insert (assoc): 100K insertions
- Map lookup: 1K lookups in 100K map
- Clone + mutate: 1K clones of 100K vec
- Filter: 100K elements
- Map merge: two 1K maps

### Before TCO (v0.4.23)

Collections were limited to ~200 elements due to stack overflow at ~200 recursive calls.
Wall-clock with 200-element collections: ~0.11s user (release).

### Improvement

| | v0.4.23 | v0.5.0 | Change |
|---|---------|--------|--------|
| Max collection size | ~200 | unlimited | stack overflow eliminated |
| Benchmark elements | 200 | 100,000 | 500x larger |
| User time (release) | 0.11s | 0.70s | 6x more time, 500x more work |

## TCO Stress Tests (v0.5.0)

Stress test script: `samples/tco-stress.oo`

### Release mode

| Run | User | System | Total |
|-----|------|--------|-------|
| 1 | 1.44s | 0.00s | 1.444s |
| 2 | 1.43s | 0.00s | 1.437s |
| 3 | 1.45s | 0.00s | 1.458s |
| **Median** | **1.44s** | **0.00s** | **1.444s** |

Tests:
- `loop`/`recur` counting to 1,000,000
- `recur` in `fn` counting down from 1,000,000
- Tail call through `if`, `match`, `do`, `when`, `try`
- `loop`/`recur` building 10K-element vector
- Mutual recursion (even/odd) to 100,000

## Asymptotic Improvements (imbl)

| Operation | Before (std Vec) | After (imbl) |
|-----------|-----------------|--------------|
| Vec clone | O(n) | O(1) |
| Vec append (conj) | O(n) | O(log n) |
| Vec prepend (cons) | O(n) | O(log n) |
| Map lookup (get) | O(n) | O(log₃₂ n) ≈ O(1) |
| Map insert (assoc) | O(n) | O(log₃₂ n) |
| Map merge | O(n*m) | O(n log m) |
| Set contains | O(n) | O(log₃₂ n) ≈ O(1) |
