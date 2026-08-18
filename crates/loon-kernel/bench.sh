#!/bin/sh
# Run the kernel N times and report the best result for each benchmark.
# Wall-clock under emulation is noisy and only ever biased upward by load, so
# the minimum is the least contaminated estimate. Allocation counts are
# deterministic and identical across runs.
N=${1:-7}
K=target/riscv64gc-unknown-none-elf/release/loon-kernel
for i in $(seq 1 "$N"); do
  timeout 300 qemu-system-riscv64 -machine virt -cpu rv64 -smp 1 -m 128M \
    -nographic -serial mon:stdio -bios default -kernel "$K" < /dev/null 2>/dev/null
done | awk '
  /^(loop|bench)/ {
    name = $1; sub(/:$/, "", name)
    for (i = 1; i <= NF; i++) if ($i == "ns/op,") { ns = $(i-1)+0 }
    for (i = 1; i <= NF; i++) if ($i == "allocs") { al = $(i-1)+0 }
    if (!(name in best) || ns < best[name]) best[name] = ns
    allocs[name] = al
  }
  END { for (n in best) printf "%-6s min %5d ns/op   %7d allocs\n", n, best[n], allocs[n] }
' | sort
