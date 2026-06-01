#!/usr/bin/env bash
# Differential test for the self-hosted ownership checker (Stage 3d).
#
# Now that ownership is wired into `loon check` (loon-cli runs OwnershipChecker
# after type-checking), it is a real oracle. For each curated snippet compare:
#   1. `loon check FILE`           — Rust type+ownership check (exit 0 = accept)
#   2. self-hosted `owns?` on FILE — reader+ownership, prints accept/reject
# Every snippet type-checks, so the only variable is ownership; reject snippets
# are additionally asserted to fail with an ownership code (E030x), not a type
# error, so the comparison is genuinely about ownership.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
lib=("$root/src/frontend/reader.oo" "$root/src/frontend/ownership.oo")

self_decision() {
  local file="$1" emit prog
  emit="$(mktemp /tmp/loon-own.XXXXXX.oo)"
  prog="$(mktemp /tmp/loon-own-full.XXXXXX.oo)"
  printf '[fn main [] [println [if [owns? [IO.read-file "%s"]] "accept" "reject"]]]\n' "$file" > "$emit"
  cat "${lib[@]}" "$emit" > "$prog"
  "$loon" run "$prog" 2>/dev/null | tail -1
  rm -f "$emit" "$prog"
}

pass=0; fail=0
for expect in accept reject; do
  for file in "$root"/src/frontend/tests/check-own/$expect/*.oo; do
    [ -e "$file" ] || continue
    name="$expect/$(basename "$file")"
    out="$("$loon" check "$file" 2>&1)"
    if "$loon" check "$file" >/dev/null 2>&1; then oracle=accept; else oracle=reject; fi
    # For reject snippets, require the oracle's failure to be an ownership code.
    if [ "$expect" = reject ] && ! echo "$out" | grep -qE "E030[012]"; then
      echo "  FAIL  $name  (oracle did not reject via E030x: $(echo "$out" | grep -oE 'E[0-9]+' | head -1))"
      fail=$((fail+1)); continue
    fi
    mine="$(self_decision "$file")"
    if [ "$oracle" = "$expect" ] && [ "$mine" = "$oracle" ]; then
      echo "  pass  $name"; pass=$((pass+1))
    else
      echo "  FAIL  $name  (expect=$expect oracle=$oracle mine=$mine)"; fail=$((fail+1))
    fi
  done
done
echo "own-diff: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "OWN-DIFF GATE: PASS" || { echo "OWN-DIFF GATE: FAIL"; exit 1; }
