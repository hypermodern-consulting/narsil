# Formatter parity harness

Differential oracle for `nix-compile fmt` against `nixfmt -` (RFC 166). The goal
is byte-exact parity; we own the formatter so we can diverge later, but first we
match.

Usage (inside `nix develop`):

    cabal build exe:nix-compile
    bash tools/fmtparity/check.sh                 # curated corpus
    bash tools/fmtparity/check.sh <dir-of-.nix>   # any corpus

`check.sh` prints `PARITY: pass/total` and a unified diff per mismatch.

`corpus/` holds curated one-construct-per-file cases (currently 14/14). Real-world
parity (flake-parts / nixpkgs fixtures) is the ongoing target; the remaining
nixfmt rules to implement are tracked in TODO #16.
