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
(nine lines, in `os/place.oo`) is wrapped around it.

| launches | no policy | place/resident | speedup |
|---------:|----------:|---------------:|--------:|
| 8 | 28.9 ms | 9.3 ms | 3.1x |
| 32 | 93.1 ms | 15.4 ms | 6.0x |
| 128 | 356.7 ms | 18.3 ms | 19.5x |

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

### Kernel time: interpreter vs GPU

| elements | interpreter | gpu | ratio |
|---------:|------------:|----:|------:|
| 1,024 | 522 µs | 10.0 ms | 0.1x |
| 16,384 | 2.7 ms | 8.7 ms | 0.3x |
| 262,144 | 40.9 ms | 13.4 ms | 3.1x |

Read this one carefully. The CPU column is **Loon's own interpreter** walking
EIR once per work item — the slowest reasonable baseline, not optimized C. The
ratio says how much there is to gain by leaving the interpreter, not how Loon's
generated shader compares to a hand-written kernel. We have not measured the
latter and do not claim it.

The GPU loses below ~100k elements, which is the expected shape: a launch is a
submission to another processor and has a fixed cost that small work cannot
amortize.

### Launch overhead

| | ns per launch |
|---|---:|
| cpu | ~7,200 |
| gpu, buffers resident | ~110,000 |

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
