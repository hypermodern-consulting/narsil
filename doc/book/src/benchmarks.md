# Benchmarks

The `narsil-bench` suite uses `tasty-bench` to measure the hot paths.

```bash
cabal run narsil-bench
```

## Groups

- **escapeForParamExpansion** — C1 escape function. Sublinear; ~300 ns for a
  malicious payload, ~1.9 μs for 100 chars of safe input.
- **parseNixExpr** — hnix wrapper. Roughly linear in source length, but the
  constant factor is high (~150 μs for a let-chain of 10).
- **analyzeDepth** — the depth precondition. Strictly linear in AST size;
  ~1 ns per node. A 150-deep app chain takes ~457 ns; a 1000-element list ~2 μs.
- **inferExprWithEnv** — HM inference (`Narsil.Nix.Inference`). Roughly
  linearithmic: the substitution is triangular (insert + resolve-on-read),
  `desugarNestedBindings` is O(n log n), and non-recursive bindings use `mapM`
  rather than `++`-append. The old super-quadratic worst case is gone — wide
  attrsets scale at about n^1.25 (the inherent `Map` cost), and let-chains stay
  near-linear (~n^1.3). Measured points (`-p inferExprWithEnv`): attrset 10 ≈
  2.7 μs, 100 ≈ 44 μs, 1000 ≈ 0.85 ms, 5000 ≈ 5.5 ms — the 5000-field case
  dropped from ~276 ms to ~5.5 ms after the RC4 fix.
- **combinedLintSafe** — single-pass lint walk. Linear in AST size; faster
  than inference by an order of magnitude.
- **safety-pipeline** — parse + analyzeDepth + infer end-to-end. Dominated by
  the parser (parser is ~95% of the time).

## Reading results

`tasty-bench` reports mean ± stdev. Example output from a recent run on the
reference machine:

```
parseNixExpr
    let-chain-10:    OK  173  μs ±  13 μs
    let-chain-50:    OK  808  μs ±  45 μs
analyzeDepth
    let-chain-10:    OK   96.0 ns ± 3.6 ns
    let-chain-50:    OK  399  ns ±  24 ns
inferExprWithEnv
    let-chain-10:    OK   17.4 μs ± 372 ns
    let-chain-50:    OK  144  μs ±  14 μs
combinedLintSafe
    let-chain-50:    OK   10.2 μs ± 907 ns
safety-pipeline
    let-chain-50:    OK  889  μs ±  42 μs
```

## Adding a benchmark

`bench/Bench.hs` is organised as `bgroup`s; add a new group by defining input
generators in the `input generators` section and adding `bench "name" $ nf fn input`
clauses to `defaultMain`. Use `parseFixture` to pre-parse inputs that should
not be counted toward the measurement.

## CI gating

Not yet wired into CI. The plan is to keep a baseline file in the repo and
fail CI if any benchmark regresses by more than 25% from the recorded baseline.
