#!/usr/bin/env bash
# CLI smoke / jank guard: run `nix-compile` in all its forms across a
# good/bad/empty/missing/dir input matrix and assert invariants. Catches the
# rough edges found by dogfooding: crashes, mislabeled/double-prefixed errors,
# and wrong exit codes. Fails (exit 1) on any regression.
#
# Usage (inside `nix develop`):
#   cabal build exe:nix-compile
#   bash tools/clicheck/check.sh "$(cabal list-bin nix-compile)"
set -uo pipefail
BIN="${1:?usage: check.sh <path-to-nix-compile>}"
W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
printf '{ a = 1; b = 2; }\n'        > "$W/good.nix"
printf '{ = }\n'                    > "$W/bad.nix"
: >                                   "$W/empty.nix"
printf '#!/usr/bin/env bash\necho hi\n' > "$W/good.sh"
mkdir -p "$W/dir"; printf '{ a = 1; }\n' > "$W/dir/x.nix"

pass=0; fail=0
# fail <desc> <reason>
note() { fail=$((fail+1)); printf 'FAIL  %s\n      %s\n' "$1" "$2"; }
ok()   { pass=$((pass+1)); }

# expect <desc> <expected-exit> <forbidden-regex> -- <args...>
expect() {
  local desc="$1" want="$2" forbid="$3"; shift 3; [ "$1" = "--" ] && shift
  local out ec; out=$("$BIN" "$@" 2>&1); ec=$?
  # never-acceptable crash/jank markers, anywhere
  if echo "$out" | grep -qiE "INTERNAL ERROR|CallStack|Prelude\.|fromJust|<<loop>>|Parse error: parse error|Parse error: I/O error"; then
    note "$desc" "crash/mislabel marker in output: $(echo "$out" | grep -iE "INTERNAL ERROR|CallStack|Prelude\.|fromJust|<<loop>>|Parse error: (parse|I/O) error" | head -1)"
    return
  fi
  if [ -n "$forbid" ] && echo "$out" | grep -qiE "$forbid"; then
    note "$desc" "forbidden pattern '$forbid' present"; return
  fi
  if [ "$want" != "*" ] && [ "$ec" != "$want" ]; then
    note "$desc" "exit $ec, wanted $want"; return
  fi
  ok
}

expect "help"            0 "" -- --help
expect "noarg"           0 "" --
expect "unknown"         1 "" -- frobnicate x
expect "fmt good"        0 "" -- fmt "$W/good.nix"
expect "fmt bad"         1 "" -- fmt "$W/bad.nix"
expect "fmt missing"     1 "" -- fmt "$W/nope.nix"
expect "fmt dir"         1 "" -- fmt "$W/dir"
expect "infer good"      0 "" -- infer "$W/good.nix"
expect "infer bad"       1 "" -- infer "$W/bad.nix"
expect "infer missing"   1 "" -- infer "$W/nope.nix"
expect "scope good"      0 "" -- scope "$W/good.nix"
expect "scope json"      0 "" -- scope --json "$W/good.nix"
expect "scope dhall"     0 "" -- scope --dhall "$W/good.nix"
expect "scope missing"   1 "" -- scope "$W/nope.nix"
expect "emit good.sh"    0 "" -- emit "$W/good.sh"
expect "emit missing"    1 "" -- emit "$W/nope.sh"
expect "check good.nix"  0 "" -- check "$W/good.nix"
expect "check bad.nix"   1 "" -- check "$W/bad.nix"
expect "check missing"   1 "" -- check "$W/nope.nix"
expect "check good.sh"   0 "" -- check "$W/good.sh"
expect "check dir"       0 "" -- check "$W/dir"

# emit must produce syntactically valid bash
if "$BIN" emit "$W/good.sh" 2>/dev/null | bash -n 2>/dev/null; then ok; else note "emit valid bash" "generated emitter fails 'bash -n'"; fi

# lsp must shut down cleanly on EOF (not hang, not crash)
lo=$(printf '' | timeout 5 "$BIN" lsp 2>&1); lec=$?
if [ "$lec" = "124" ]; then note "lsp eof" "lsp hung on empty stdin (timeout)"
elif echo "$lo" | grep -qiE "INTERNAL ERROR|CallStack|Prelude\.|fromJust"; then note "lsp eof" "lsp crashed on empty stdin"
else ok; fi

# error-category invariants: missing/dir are I/O errors, not parse errors
mfo=$("$BIN" fmt "$W/nope.nix" 2>&1)
echo "$mfo" | grep -qiE "I/O error" || note "fmt missing label" "expected 'I/O error', got: $mfo"
echo "$mfo" | grep -qiE "I/O error" && ok
dfo=$("$BIN" fmt "$W/dir" 2>&1)
echo "$dfo" | grep -qiE "I/O error" || note "fmt dir label" "expected 'I/O error', got: $dfo"
echo "$dfo" | grep -qiE "I/O error" && ok

# ── stdout/stderr contract (output rework C0) ───────────────────────────────
# Data commands: product on stdout, nothing on stderr on success.
so=$("$BIN" fmt "$W/good.nix" 2>"$W/e"); se=$(cat "$W/e")
{ [ -n "$so" ] && [ -z "$se" ]; } && ok || note "fmt stream contract" "stdout empty or stderr noisy (stderr='$se')"
so=$("$BIN" scope --json "$W/good.nix" 2>"$W/e"); se=$(cat "$W/e")
{ [ -n "$so" ] && [ -z "$se" ]; } && ok || note "scope --json stream contract" "stderr not empty: '$se'"
# Diagnostics: check emits findings to stderr, never stdout.
printf 'foo bar baz\n' > "$W/bare.sh"
so=$("$BIN" check "$W/bare.sh" 2>/dev/null)
[ -z "$so" ] && ok || note "check stdout clean" "diagnostics leaked to stdout: '$so'"
se=$("$BIN" check "$W/bare.sh" 2>&1 >/dev/null)
echo "$se" | grep -qiE "bare command|ALEPH" && ok || note "check diag on stderr" "expected bare-command diagnostic on stderr"

echo "CLICHECK: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
