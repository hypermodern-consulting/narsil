#!/usr/bin/env bash
# Parity harness: compare `nix-compile fmt` against `nixfmt -` over a corpus.
# Usage: check.sh [corpus-dir]   (run inside `nix develop`)
set -uo pipefail
CORPUS="${1:-/tmp/fmtparity/corpus}"
BIN=$(cabal list-bin nix-compile 2>/dev/null)
pass=0; fail=0; failed=()
for f in "$CORPUS"/*.nix; do
  exp=$(nixfmt - < "$f" 2>/dev/null)
  act=$("$BIN" fmt "$f" 2>/dev/null)
  if [ "$exp" == "$act" ]; then
    pass=$((pass+1))
  else
    fail=$((fail+1)); failed+=("$f")
  fi
done
echo "PARITY: $pass/$((pass+fail))"
for f in "${failed[@]:-}"; do
  [ -z "$f" ] && continue
  echo "════ MISMATCH: $(basename "$f") ════"
  diff <(nixfmt - < "$f" 2>/dev/null) <("$BIN" fmt "$f" 2>/dev/null) | sed 's/^/  /'
done
