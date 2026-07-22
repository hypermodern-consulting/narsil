#!/usr/bin/env bash
# End-to-end layout-enforcement guard: run the REAL `narsil` binary against
# two complete flake-parts projects (test/fixtures/layout-projects/) and assert
# that the wired `check <dir>` path actually enforces the directory-layout
# convention — clean project => no layout errors; perturbed project => the
# expected E0xx layout diagnostics and a non-zero exit. Fails (exit 1) on any
# regression. This is the self-dogfooding coverage the unit fixtures can't give:
# it exercises CLI -> cmdCI -> runLayoutPhase end to end.
#
# This is run ONLY by `nix flake check`, via the `narsil:layout-e2e` check
# in flake-modules/default.nix — a single disciplined runner so it can't drift.
# To run just this check during development:
#   nix build .#checks.x86_64-linux."narsil:layout-e2e" -L
set -uo pipefail
BIN="${1:?usage: check.sh <path-to-narsil>}"
HERE="$(cd "$(dirname "$0")/../.." && pwd)"
GOOD="$HERE/test/fixtures/layout-projects/good"
PERT="$HERE/test/fixtures/layout-projects/perturbed"

# any layout diagnostic code E001..E010
LAYOUT='error\[E0(0[1-9]|10)\]'

pass=0; fail=0
note() { fail=$((fail+1)); printf 'FAIL  %s\n      %s\n' "$1" "$2"; }
ok()   { pass=$((pass+1)); }

# run `check <proj>` using that project's own config (so its layout= is honored)
runp() { "$BIN" --config "$1/.narsil.dhall" check "$1" 2>&1; }

# 1) the clean project must emit NO layout diagnostics
good_out="$(runp "$GOOD")"
if echo "$good_out" | grep -qE "$LAYOUT"; then
  note "good project is layout-clean" "unexpected: $(echo "$good_out" | grep -E "$LAYOUT" | head -1)"
else ok; fi

# 2) the perturbed project must emit the specific violations we planted
pert_out="$(runp "$PERT")"
want_code() {
  if echo "$pert_out" | grep -qE "error\[$1\]"; then ok
  else note "perturbed emits $1" "missing $1 in output"; fi
}
want_code E001   # FlakeModule at root + Package under lib/ (wrong location)
want_code E007   # banned _index.nix

# 3) at least three layout violations total (E001 x2 + E007)
n="$(echo "$pert_out" | grep -cE "$LAYOUT")"
if [ "$n" -ge 3 ]; then ok; else note "perturbed violation count" "got $n, want >=3"; fi

# 4) perturbed must exit non-zero (layout violations alone force failure)
"$BIN" --config "$PERT/.narsil.dhall" check "$PERT" >/dev/null 2>&1
[ $? -ne 0 ] && ok || note "perturbed exits non-zero" "exit 0 despite violations"

printf 'LAYOUTCHECK: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
