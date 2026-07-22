#!/usr/bin/env bash
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                        // ci // oracle sweep
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# The full-nixpkgs differential oracle sweep, CI-shaped: fetch the flake-pinned
# nixpkgs into the store, run every file through the checker, and compare
# against the committed ratchet baseline (test/oracle/nixpkgs-baseline.json).
# Crashes/hangs are always fatal; counted buckets may only shrink.
#
# ~25 minutes on 8 cores for the full corpus. Pass --sample N for a quick
# smoke (crash/hang enforcement only — counts skip on partial corpora).
set -euo pipefail
cd "$(dirname "$0")/.."

# Realize the pinned nixpkgs source in the store (the sweep resolves the same
# pin from flake.lock; `archive` just guarantees it is present).
nix flake archive --json >/dev/null

exec nix develop -c cabal run -v0 narsil-oracle-sweep -- "$@"
