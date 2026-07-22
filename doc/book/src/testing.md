# The Apparatus

A checker's value is exactly the trust you can place in its output, so
narsil's verification machinery is not an afterthought — it is co-equal
with the engine. Four independent legs, each covering a failure mode the
others cannot see, all enforced in CI.

## Leg 1: the ratcheted corpus sweep

`narsil-oracle-sweep` runs the full check pipeline over **every `.nix`
file in a pinned nixpkgs** — 43,170 files — and compares the outcome
distribution against a committed baseline
(`test/oracle/nixpkgs-baseline.json`):

- **crashes and hangs are always fatal** (the current count of each is 0);
- **counted error buckets may only shrink** — a change that introduces a
  new false positive class fails the sweep;
- a corpus fingerprint guards against comparing across different pins.

The premise is the differential-oracle property: *stock nixpkgs is
well-typed* — `nix-instantiate` accepts it — so any file narsil rejects is
presumed narsil's bug until classified otherwise. Current baseline: 43,134
type-ok (99.92%), 28 diagnostics (0.065%), each individually classified in
[The Contract](./contract.md).

## Leg 2: the mutation ledger

The ratchet only measures false positives, and that pressure pushes any
checker toward accepting everything. The mutation ledger
(`test/MutationSpec.hs`) holds the opposite line. Each entry is a seed
expression the checker accepts and a mutation of it:

- **19 `MustCatch` entries** — the mutant is a genuine bug shape
  (`p.pnmae`, `stringLength 5`, `[1] + [2]`, an enum definition violating
  its declared type). If one starts *passing*, a leniency change went too
  far — a regression even though no corpus number moved.
- **7 `AcceptedByDesign` entries** — the ledger of deliberate leniency
  trades (defaulted fields, null placeholders, lazy self-reference). If one
  starts being *caught*, the entry is stale: promote it and celebrate.

Every deliberate trade-off in the engine has a witness here; a change that
cannot say which entry it is trading against does not land.

## Leg 3: the generative differential fuzzer

`narsil-oracle-fuzz` generates **well-typed-by-construction** terms from a
type-directed grammar (scope-correct lets, β-redexes, record projections,
operator towers) and requires narsil and `nix-instantiate` to agree on
every single one. A disagreement in either direction — a false positive on
a term the generator proves well-typed, or a crash — is fatal. Seeded and
reproducible (`--seed`, `--count`); CI runs a thousand fresh terms nightly.

## Leg 4: golden oracles and property tests

The `narsil-test` suite — **611 tests** — bundles QuickCheck properties
over every subsystem, frozen golden files for a hand-curated differential
corpus (with a live drift-check against `nix`), regression pins for every
false-positive class ever fixed, LSP feature contracts, and Dhall↔Haskell
**parity tests** that refuse to let the profile and rule tables drift from
their `config/*.dhall` sources of truth.

One idiom worth stealing: the **tripwire**. A known-but-unfixed gap is
encoded as its correct contract, *inverted* — green while the bug lives,
red the moment someone fixes it, which is the signal to promote it to a
permanent guard. The suite never lies about what works.

## Running it

```bash
cabal test narsil-test          # the 611
cabal test narsil-oracle        # golden differential corpus
cabal run narsil-oracle-fuzz -- --seed 42 --count 1000
scripts/ci-sweep.sh             # the full-nixpkgs sweep (~25 min)
nix flake check                 # everything, plus format/style gates
```

The sweep supports `--dump-errors <path>` (a TSV of every diagnostic for
mining) and `--write-baseline` (ratchet after verified improvement).
