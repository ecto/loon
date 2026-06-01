#!/usr/bin/env bash
# Third-pass self-application gate: run the self-hosted TYPE CHECKER (infer) on
# the self-hosted VM and check its accept/reject decisions match native loon.
#
# The guest program is reader.oo ++ types.oo ++ infer.oo ++ driver. For each
# driver in tests/infer-vm/ (which prints checks? accept/reject for a set of
# programs):
#   oracle = loon run (reader.oo ++ types.oo ++ infer.oo ++ driver)   — native
#   mine   = loon run (reader+eir+lower+vm ++ wrapper), where the wrapper
#            eir-run-str's the same four files' text on the VM
# Equal stdout means the lowered type checker — unification, the inference
# context, ADTs, maps/vectors, recursion and the whole ~700-line pass — runs on
# the register VM with the same decisions as the Rust toolchain.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
guest=("$root/src/frontend/reader.oo" "$root/src/frontend/types.oo" "$root/src/frontend/infer.oo")
lib=("$root/src/frontend/reader.oo" "$root/src/frontend/eir.oo" "$root/src/frontend/lower.oo" "$root/src/frontend/vm.oo")

pass=0; fail=0
for driver in "$root"/src/frontend/tests/infer-vm/*.oo; do
  name="$(basename "$driver")"
  orc="$(mktemp /tmp/iv-orc.XXXXXX.oo)"
  cat "${guest[@]}" "$driver" > "$orc"
  oracle="$("$loon" run "$orc" 2>&1)"
  wrap="$(mktemp /tmp/iv-wrap.XXXXXX.oo)"
  full="$(mktemp /tmp/iv-full.XXXXXX.oo)"
  printf '[fn main [] [eir-run-str [str [IO.read-file "%s"] [IO.read-file "%s"] [IO.read-file "%s"] [IO.read-file "%s"]]]]\n' \
    "${guest[0]}" "${guest[1]}" "${guest[2]}" "$driver" > "$wrap"
  cat "${lib[@]}" "$wrap" > "$full"
  mine="$("$loon" run "$full" 2>&1)"
  rm -f "$orc" "$wrap" "$full"
  if [ "$oracle" = "$mine" ]; then
    echo "  pass  $name"; pass=$((pass+1))
  else
    echo "  FAIL  $name"; fail=$((fail+1))
    echo "    oracle: $(echo "$oracle" | tr '\n' '|')"
    echo "    mine:   $(echo "$mine" | tr '\n' '|')"
  fi
done
echo "infer-vm: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "INFER-VM GATE: PASS" || { echo "INFER-VM GATE: FAIL"; exit 1; }
