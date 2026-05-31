#!/usr/bin/env bash
# Corpus harness for the self-hosted frontend.
#
# The EIR VM has no working module `use`, so we amalgamate by concatenation:
#   1. the frontend library files (reader, and for fmt also the formatter),
#   2. a generated `corpus-files` function listing every .oo/.loon source,
#   3. the chosen test driver.
# Then run it on the Rust-hosted Loon and print the ship-gate result.
#
# Usage:
#   src/frontend/run-corpus.sh read   # Stage-0 reader round-trip gate
#   src/frontend/run-corpus.sh fmt    # Stage-1 formatter idempotence+round-trip
set -euo pipefail

mode="${1:-read}"
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${2:-$root/target/debug/loon}"

case "$mode" in
  read) libs=("$root/src/frontend/reader.oo")
        driver="$root/src/frontend/tests/corpus_driver.oo" ;;
  fmt)  libs=("$root/src/frontend/reader.oo" "$root/src/frontend/comments.oo" "$root/src/frontend/formatter.oo")
        driver="$root/src/frontend/tests/fmt_corpus_driver.oo" ;;
  *) echo "usage: $0 {read|fmt} [loon-binary]" >&2; exit 2 ;;
esac

gen="$(mktemp /tmp/loon-corpus-list.XXXXXX.oo)"
prog="$(mktemp /tmp/loon-frontend.XXXXXX.oo)"
trap 'rm -f "$gen" "$prog"' EXIT

{
  printf '[fn corpus-files []\n  #[\n'
  cd "$root"
  find samples web/src crates docs -type f \( -name '*.oo' -o -name '*.loon' \) \
    | LC_ALL=C sort \
    | while IFS= read -r f; do printf '    "%s"\n' "$f"; done
  printf '  ]]\n'
} > "$gen"

cat "${libs[@]}" "$gen" "$driver" > "$prog"
cd "$root"
"$loon" run "$prog"
