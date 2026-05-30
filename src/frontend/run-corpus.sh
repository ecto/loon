#!/usr/bin/env bash
# Stage-0 corpus round-trip harness.
#
# Builds a single runnable Loon program (the EIR VM has no working module
# `use`, so we concatenate) from:
#   1. the reader library,
#   2. a generated `corpus-files` function listing every .oo/.loon source,
#   3. the corpus driver.
# Then runs it on the Rust-hosted Loon and prints the ship-gate result.
#
# Usage: src/frontend/run-corpus.sh [loon-binary]
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"

gen="$(mktemp /tmp/loon-corpus-list.XXXXXX.oo)"
prog="$(mktemp /tmp/loon-stage0.XXXXXX.oo)"
trap 'rm -f "$gen" "$prog"' EXIT

# Generate `[fn corpus-files [] #[ "path" ... ]]` over the whole repo corpus.
{
  printf '[fn corpus-files []\n  #[\n'
  cd "$root"
  find samples web/src crates docs -type f \( -name '*.oo' -o -name '*.loon' \) \
    | LC_ALL=C sort \
    | while IFS= read -r f; do printf '    "%s"\n' "$f"; done
  printf '  ]]\n'
} > "$gen"

cat "$root/src/frontend/reader.oo" "$gen" "$root/src/frontend/tests/corpus_driver.oo" > "$prog"

cd "$root"
"$loon" run "$prog"
