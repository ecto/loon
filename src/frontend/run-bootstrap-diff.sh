#!/usr/bin/env bash
# THE BOOTSTRAP gate (slow): the self-hosted VM running the self-hosted VM.
#
# Layers:
#   host loon  ->  VM (layer 1, interpreting vm.oo)  ->  VM (layer 2)  ->  P
# The layer-1 VM lowers and runs the ENTIRE backend (reader+eir+lower+vm) plus an
# inner wrapper; that inner program is itself a VM that lowers and runs P. So P's
# result is produced by the VM executing the VM — a meta-circular bootstrap.
#
#   oracle = loon run prog.oo                         (P run natively)
#   mine   = loon run (backend ++ layer-1 wrapper)    (P run by VM-on-VM)
# Equal output means the self-hosted backend correctly executes itself.
#
# NOTE: deliberately heavy (interpreter-squared, ~30s) — not part of the fast
# gate set. P (tests/bootstrap/prog.oo) must be a single line with no double
# quotes so it embeds cleanly as a string literal in the inner wrapper.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
loon="${1:-$root/target/debug/loon}"
backend=("$root/src/frontend/reader.oo" "$root/src/frontend/eir.oo" \
         "$root/src/frontend/lower.oo" "$root/src/frontend/vm.oo")
prog="$root/src/frontend/tests/bootstrap/prog.oo"

oracle="$("$loon" run "$prog" 2>&1)"

iw="$(mktemp /tmp/bs-iw.XXXXXX.oo)"
printf '[fn main [] [eir-run-str "%s"]]\n' "$(cat "$prog")" > "$iw"
wrap="$(mktemp /tmp/bs-wrap.XXXXXX.oo)"
{
  printf '[fn main [] [eir-run-str [str\n'
  for f in "${backend[@]}" "$iw"; do printf '  [IO.read-file "%s"]\n' "$f"; done
  printf ']]]\n'
} > "$wrap"
full="$(mktemp /tmp/bs-full.XXXXXX.oo)"
cat "${backend[@]}" "$wrap" > "$full"
mine="$("$loon" run "$full" 2>&1)"
rm -f "$iw" "$wrap" "$full"

echo "  oracle (native):     $oracle"
echo "  bootstrap (VM-on-VM): $mine"
if [ "$oracle" = "$mine" ]; then
  echo "BOOTSTRAP GATE: PASS"
else
  echo "BOOTSTRAP GATE: FAIL"; exit 1
fi
