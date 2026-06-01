#!/usr/bin/env bash
# F2 (self-application) gate: run the self-hosted READER on the self-hosted VM
# and check it parses + reprints exactly as native `loon run` does.
#
# For each driver in tests/f2/ (a small program that read-all's an input and
# write-form's the result):
#   oracle = loon run (reader.oo ++ driver)            — native reader
#   mine   = loon run (reader+eir+lower+vm ++ wrapper), where the wrapper uses
#            eir-run-str to lower and run (reader.oo ++ driver) text on the VM
# Reading both source files verbatim (no string interpolation) avoids any
# quoting hazards. Equal stdout means the lowered reader, executing on the
# self-hosted register VM — through effects (spans/diagnostics), ADTs,
# loop/recur, strings and keywords — behaves identically to the Rust toolchain.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
reader="$root/src/frontend/reader.oo"
lib=("$reader" "$root/src/frontend/eir.oo" "$root/src/frontend/lower.oo" "$root/src/frontend/vm.oo")

pass=0; fail=0
for driver in "$root"/src/frontend/tests/f2/*.oo; do
  name="$(basename "$driver")"
  orc="$(mktemp /tmp/f2-orc.XXXXXX.oo)"
  cat "$reader" "$driver" > "$orc"
  oracle="$("$loon" run "$orc" 2>&1)"
  wrap="$(mktemp /tmp/f2-wrap.XXXXXX.oo)"
  full="$(mktemp /tmp/f2-full.XXXXXX.oo)"
  printf '[fn main [] [eir-run-str [str [IO.read-file "%s"] [IO.read-file "%s"]]]]\n' "$reader" "$driver" > "$wrap"
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
echo "f2-diff: pass=$pass fail=$fail"
[ "$fail" -eq 0 ] && echo "F2 GATE: PASS" || { echo "F2 GATE: FAIL"; exit 1; }
