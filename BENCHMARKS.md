# Benchmarks

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
