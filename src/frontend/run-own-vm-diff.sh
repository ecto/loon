#!/usr/bin/env bash
# Fourth-pass self-application gate: run the self-hosted OWNERSHIP/BORROW CHECKER
# on the self-hosted VM and check its accept/reject decisions match native loon.
# Guest = reader.oo ++ ownership.oo ++ driver (prints owns? ok/bad per program).
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
guest=("$root/src/frontend/reader.oo" "$root/src/frontend/ownership.oo")
lib=("$root/src/frontend/reader.oo" "$root/src/frontend/eir.oo" "$root/src/frontend/lower.oo" "$root/src/frontend/vm.oo")
pass=0; fail=0
for driver in "$root"/src/frontend/tests/own-vm/*.oo; do
  name="$(basename "$driver")"
  orc="$(mktemp /tmp/ov-orc.XXXXXX.oo)"
  cat "${guest[@]}" "$driver" > "$orc"
  oracle="$("$loon" run "$orc" 2>&1)"
  wrap="$(mktemp /tmp/ov-wrap.XXXXXX.oo)"
  full="$(mktemp /tmp/ov-full.XXXXXX.oo)"
  printf '[fn main [] [eir-run-str [str [IO.read-file "%s"] [IO.read-file "%s"] [IO.read-file "%s"]]]]\n' \
    "${guest[0]}" "${guest[1]}" "$driver" > "$wrap"
  cat "${lib[@]}" "$wrap" > "$full"
  mine="$("$loon" run "$full" 2>&1)"
  rm -f "$orc" "$wrap" "$full"
  if [ "$oracle" = "$mine" ]; then echo "  pass  $name"; pass=$((pass+1));
  else echo "  FAIL  $name"; fail=$((fail+1)); echo "    oracle: $(echo "$oracle" | tr '\n' '|')"; echo "    mine:   $(echo "$mine" | tr '\n' '|')"; fi
done
echo "own-vm: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "OWN-VM GATE: PASS" || { echo "OWN-VM GATE: FAIL"; exit 1; }
