#!/usr/bin/env bash
# Differential test for the self-hosted macro expander.
#
# Each program in tests/macros/*.oo uses macros and prints deterministic output.
# We run it two ways and compare stdout:
#   1. `loon run PROG`                       — the Rust expander expands, then runs
#   2. self-hosted expand -> write macro-free source -> `loon run`
# Equal output means the self-hosted expander is behaviorally equivalent to the
# Rust expander on that program.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
lib=("$root/src/frontend/reader.oo" "$root/src/frontend/expander.oo")

pass=0; fail=0
for prog in "$root"/src/frontend/tests/macros/*.oo; do
  name="$(basename "$prog")"
  rust_out="$("$loon" run "$prog" 2>&1)"; rust_rc=$?
  # self-hosted: emit the expanded, macro-free program source
  emit="$(mktemp /tmp/loon-emit.XXXXXX.oo)"
  prog_out="$(mktemp /tmp/loon-expanded.XXXXXX.oo)"
  printf '[fn main [] [print [write-program [expand-all [read-all [IO.read-file "%s"]]]]]]\n' "$prog" > "$emit"
  cat "${lib[@]}" "$emit" > "${emit}.full"
  "$loon" run "${emit}.full" > "$prog_out" 2>/dev/null
  mine_out="$("$loon" run "$prog_out" 2>&1)"; mine_rc=$?
  if [ "$rust_out" = "$mine_out" ] && [ "$rust_rc" -eq "$mine_rc" ]; then
    echo "  pass  $name"; pass=$((pass+1))
  else
    echo "  FAIL  $name"; fail=$((fail+1))
    echo "    rust($rust_rc): $(echo "$rust_out" | tr '\n' '|')"
    echo "    mine($mine_rc): $(echo "$mine_out" | tr '\n' '|')"
    echo "    expanded: $(cat "$prog_out" | tr '\n' '|')"
  fi
  rm -f "$emit" "${emit}.full" "$prog_out"
done
echo "differential: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "DIFFERENTIAL GATE: PASS" || echo "DIFFERENTIAL GATE: FAIL"
