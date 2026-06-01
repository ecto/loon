#!/usr/bin/env bash
# Inline (no-IO, self-contained) test harness for the self-hosted frontend.
#
# The EIR VM has no working module `use`, so we amalgamate by concatenation:
# the relevant library files followed by the stage's inline driver, then run on
# the Rust-hosted Loon. Each driver prints "  pass "/"  FAIL " lines; this
# script turns the presence of any FAIL into a non-zero exit + gate line.
#
# Usage:
#   src/frontend/run-inline.sh read    # Stage-0 reader
#   src/frontend/run-inline.sh expand  # Stage-2 expander
#   src/frontend/run-inline.sh types   # Stage-3a types / HM core
set -euo pipefail

mode="${1:-types}"
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${2:-$root/target/debug/loon}"

case "$mode" in
  read)   libs=("$root/src/frontend/reader.oo")
          driver="$root/src/frontend/tests/inline.oo" ;;
  expand) libs=("$root/src/frontend/reader.oo" "$root/src/frontend/expander.oo")
          driver="$root/src/frontend/tests/expand_inline.oo" ;;
  types)  libs=("$root/src/frontend/reader.oo" "$root/src/frontend/types.oo")
          driver="$root/src/frontend/tests/types_inline.oo" ;;
  *) echo "usage: $0 {read|expand|types} [loon-binary]" >&2; exit 2 ;;
esac

prog="$(mktemp /tmp/loon-inline.XXXXXX.oo)"
trap 'rm -f "$prog"' EXIT
cat "${libs[@]}" "$driver" > "$prog"

out="$("$loon" run "$prog")"
echo "$out"
if echo "$out" | grep -q "FAIL"; then
  echo "INLINE GATE ($mode): FAIL"; exit 1
else
  echo "INLINE GATE ($mode): PASS"
fi
