#!/usr/bin/env bash
# Second-pass self-application gate: run the self-hosted EXPANDER on the
# self-hosted VM and check it expands macros exactly as native `loon run` does.
#
# The guest program is reader.oo ++ expander.oo ++ driver (the expander is
# concatenated after the reader). For each driver in tests/expand-vm/:
#   oracle = loon run (reader.oo ++ expander.oo ++ driver)        — native
#   mine   = loon run (reader+eir+lower+vm ++ wrapper), where the wrapper
#            eir-run-str's the same three files' text on the VM
# Equal stdout means the lowered expander — running on the register VM through
# quasiquote expansion, hygiene scopes, the Expand.error effect, ADTs, maps/
# vectors, and recursion — matches the Rust toolchain.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
reader="$root/src/frontend/reader.oo"
expander="$root/src/frontend/expander.oo"
lib=("$reader" "$root/src/frontend/eir.oo" "$root/src/frontend/lower.oo" "$root/src/frontend/vm.oo")

pass=0; fail=0
for driver in "$root"/src/frontend/tests/expand-vm/*.oo; do
  name="$(basename "$driver")"
  orc="$(mktemp /tmp/xv-orc.XXXXXX.oo)"
  cat "$reader" "$expander" "$driver" > "$orc"
  oracle="$("$loon" run "$orc" 2>&1)"
  wrap="$(mktemp /tmp/xv-wrap.XXXXXX.oo)"
  full="$(mktemp /tmp/xv-full.XXXXXX.oo)"
  printf '[fn main [] [eir-run-str [str [IO.read-file "%s"] [IO.read-file "%s"] [IO.read-file "%s"]]]]\n' \
    "$reader" "$expander" "$driver" > "$wrap"
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
echo "expand-vm: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "EXPAND-VM GATE: PASS" || { echo "EXPAND-VM GATE: FAIL"; exit 1; }
