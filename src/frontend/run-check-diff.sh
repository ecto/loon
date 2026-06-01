#!/usr/bin/env bash
# Differential test for the self-hosted type checker (Stage 3b).
#
# For each curated snippet, compare the accept/reject decision of:
#   1. `loon check FILE`              — the Rust checker (oracle; exit 0 = accept)
#   2. self-hosted `checks?` on FILE  — reader+types+infer, prints accept/reject
# Equal decisions on every file means the self-hosted checker agrees with the
# oracle over the covered language subset.
#
# Snippets live in tests/check/{accept,reject}/; the subdir is the expected
# decision and is also asserted against the oracle (a guard against the oracle
# drifting). Coverage grows toward the full corpus as 3b expands.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
lib=("$root/src/frontend/reader.oo" "$root/src/frontend/types.oo" "$root/src/frontend/infer.oo")

# Self-hosted decision for one file: "accept" or "reject".
self_decision() {
  local file="$1"
  local emit prog
  emit="$(mktemp /tmp/loon-chk.XXXXXX.oo)"
  prog="$(mktemp /tmp/loon-chk-full.XXXXXX.oo)"
  printf '[fn main [] [println [if [checks? [IO.read-file "%s"]] "accept" "reject"]]]\n' "$file" > "$emit"
  cat "${lib[@]}" "$emit" > "$prog"
  "$loon" run "$prog" 2>/dev/null | tail -1
  rm -f "$emit" "$prog"
}

pass=0; fail=0
for expect in accept reject; do
  for file in "$root"/src/frontend/tests/check/$expect/*.oo; do
    [ -e "$file" ] || continue
    name="$expect/$(basename "$file")"
    if "$loon" check "$file" >/dev/null 2>&1; then oracle=accept; else oracle=reject; fi
    mine="$(self_decision "$file")"
    if [ "$oracle" = "$expect" ] && [ "$mine" = "$oracle" ]; then
      echo "  pass  $name"; pass=$((pass+1))
    else
      echo "  FAIL  $name  (expect=$expect oracle=$oracle mine=$mine)"; fail=$((fail+1))
    fi
  done
done
echo "check-diff: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "CHECK-DIFF GATE: PASS" || { echo "CHECK-DIFF GATE: FAIL"; exit 1; }
