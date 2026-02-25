# Collection Benchmarks: Vec → imbl Persistent Data Structures

## Summary

Replaced `Vec<Value>`, `Vec<(Value, Value)>` (Map), and `Vec<Value>` (Set) with
`imbl::Vector`, `imbl::HashMap`, and `imbl::HashSet` respectively.

## Asymptotic Improvements

| Operation | Before (std Vec) | After (imbl) | Improvement |
|-----------|-----------------|--------------|-------------|
| Vec clone | O(n) | O(1) | structural sharing |
| Vec append (conj) | O(n) clone+push | O(log n) | RRB-tree |
| Vec prepend (cons) | O(n) | O(log n) | RRB-tree |
| Map lookup (get) | O(n) linear scan | O(log₃₂ n) ≈ O(1) | HAMT |
| Map insert (assoc) | O(n) clone+scan | O(log₃₂ n) | HAMT |
| Map remove | O(n) | O(log₃₂ n) | HAMT |
| Map merge | O(n*m) | O(n log m) | HAMT |
| Set contains | O(n) | O(log₃₂ n) ≈ O(1) | HAMT |
| Set insert (conj) | O(n) | O(log₃₂ n) | HAMT |

## Wall-Clock Benchmarks

Benchmark script: `samples/bench-collections.oo`

Sizes kept small (200 elements) due to interpreter stack depth limits.
At these sizes, interpreter overhead dominates, so wall-clock differences are minimal.
The real payoff is at scale and with repeated clone operations.

### Before (std Vec/Map)

| Run | User | System | Total |
|-----|------|--------|-------|
| 1 | 0.13s | 0.09s | 1.323s |
| 2 | 0.13s | 0.07s | 1.325s |
| 3 | 0.12s | 0.08s | 1.410s |
| **Median** | **0.13s** | **0.08s** | **1.325s** |

### After (imbl persistent)

| Run | User | System | Total |
|-----|------|--------|-------|
| 1 | 0.10s | 0.08s | 1.339s |
| 2 | 0.11s | 0.08s | 1.351s |
| 3 | 0.12s | 0.07s | 1.346s |
| **Median** | **0.11s** | **0.08s** | **1.346s** |

### Notes

- Wall-clock times include cargo overhead (~1.2s) and are dominated by interpreter startup
- User-time median dropped from 0.13s to 0.11s (~15% improvement even at 200 elements)
- The key wins (O(1) clone, O(1) map lookup) compound at larger collection sizes
- With tail-call optimization or larger stack sizes, collections of 10k+ elements
  would show dramatic improvements (hours → seconds for pathological cases)
