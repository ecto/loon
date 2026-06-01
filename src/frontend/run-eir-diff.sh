#!/usr/bin/env bash
# Behavioral differential for the self-hosted EIR backend (Stage 3e/3f) — the F1
# fixpoint criterion on the supported subset.
#
# For each program, compare stdout of:
#   1. `loon run FILE`               — the Rust toolchain (oracle)
#   2. self-hosted reader->lower->VM — a driver that reads FILE and eir-run-str's
#      it (main's println side effects produce the output)
# Equal stdout means the self-hosted backend is behaviorally equivalent to
# `loon run` on that program. Programs live in tests/eir/ and stay within the
# lowered subset (literals, arithmetic, if, let, calls/recursion, println/str).
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
lib=("$root/src/frontend/reader.oo" "$root/src/frontend/eir.oo" \
     "$root/src/frontend/lower.oo" "$root/src/frontend/vm.oo")

pass=0; fail=0
for prog in "$root"/src/frontend/tests/eir/*.oo; do
  name="$(basename "$prog")"
  oracle="$("$loon" run "$prog" 2>&1)"
  emit="$(mktemp /tmp/loon-eir.XXXXXX.oo)"
  full="$(mktemp /tmp/loon-eir-full.XXXXXX.oo)"
  printf '[fn main [] [eir-run-str [IO.read-file "%s"]]]\n' "$prog" > "$emit"
  cat "${lib[@]}" "$emit" > "$full"
  mine="$("$loon" run "$full" 2>&1)"
  rm -f "$emit" "$full"
  if [ "$oracle" = "$mine" ]; then
    echo "  pass  $name"; pass=$((pass+1))
  else
    echo "  FAIL  $name"; fail=$((fail+1))
    echo "    oracle: $(echo "$oracle" | tr '\n' '|')"
    echo "    mine:   $(echo "$mine" | tr '\n' '|')"
  fi
done
echo "eir-diff: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "EIR-DIFF GATE: PASS" || { echo "EIR-DIFF GATE: FAIL"; exit 1; }
