# Testing

narsil ships six test suites. The main property suite (`narsil-test`)
bundles the QuickCheck properties, the "psychotic" review-2 regression suite, the
LSP project-cache spec, and the bash/Nix adversarial suites; the full run reports
**446/446 passing**. Alongside it are three fixture suites and a differential
soundness oracle that checks inferred types against `nix-instantiate`.

## Running tests

```bash
# All suites
cabal test

# Property + regression + adversarial suite
cabal test narsil-test

# Fixture tests
cabal test narsil-fixtures
cabal test narsil-more-fixtures
cabal test narsil-flake-parts

# Differential soundness oracle (vs nix-instantiate builtins.typeOf)
cabal test narsil-oracle
```

## Test suites

### `narsil-test` (Props.hs + Adversarial.hs + NixAdversarial.hs + Psychotic.hs + ProjectCacheSpec.hs)

`Props.hs` registers the QuickCheck property groups (a long `run "name" prop`
list) and then folds in the wired-in adversarial suites and the psychotic and
project-cache regression suites. The registered groups cover, roughly:

| Group              | What it tests                                                                            |
| ------------------ | --------------------------------------------------------------------------------------- |
| Type algebra       | Unification reflexive/symmetric/disjoint, substitution composition, order-independence  |
| Constraints        | Empty identity, reflexive, satisfaction, deterministic solving                          |
| Fact extraction    | DefaultIs, Required, AssignFrom, ConfigAssign/Lit vectors, config quoting/splitting      |
| Schema building    | Complete, defaults preserved, required marked, defaulted-vars diagnostic                |
| Merge correctness  | Required preserved, default kept, duplicate merged, identity law                        |
| Parser / patterns  | Labeled success/failure, empty, comments; `${VAR:-}`/`${VAR:?}`/`$VAR`, numeric vectors |
| Builtins           | Database integrity, known flag types, unknown flag/command handling                     |
| Config tree        | Completeness over conflict-free path sets                                               |
| Scope graph        | Edge priority, let/attrset/func/with/var construction, cross-file merge                 |
| Nix inference      | Totality, determinism, literal types, lists, attrsets, functions, let, `with`, `rec`    |
| Nix / bash lint    | `with`/`rec` detection, derivation/package/pattern lint rules, heredoc/backtick         |
| Emit-config        | `${VAR:?}` guards, no null, literal/string quoting, balanced JSON, YAML/TOML, nested    |
| Format             | Type-annotation smoke test (`annotateExpr` injects an annotation)                       |
| REVIEW-3 round-3   | Nested-select errors, polymorphic builtins terminate, `+`/`toString` typing, reformatter round-trip, well-typed exact-type vectors, row-polymorphic attribute builtins, cross-module import type flow |
| Layout / naming    | Layout-convention validation, kebab/camel/pascal naming, glob matching                  |
| Severity / rules   | Severity overrides, rule-id uniqueness, suppression                                     |
| LSP                | Lint diagnostics, hover, definition, references, rename, completion                     |
| CLI                | Report partitioning/formatting, `check` unsupported-construct detection, CI markers     |
| Overlay algebra    | Identity, associativity, satisfaction, propagation                                      |
| E2E / stress / edge| Config extraction, required vars, type conflicts, store paths; large/many-var scripts   |

The `Format` group is now a single smoke test (`format_function`) that confirms
the type-annotation renderer injects an annotation; meaning-preservation and the
reformatter are covered by the REVIEW-3 round-trip properties below. The renderer
under test is `Narsil.Nix.Infer.annotateExpr` (the `infer` command), formerly
`formatExpr`.

New round-3 properties worth calling out: `reformatter_roundtrip` /
`reformatter_roundtrip_corpus` / `reformatter_indented_string` exercise the
vendored-nixfmt reformatter as real passing meaning-preserving round-trips
(including indented strings); `welltyped_vectors` pins exact inferred types for a
fixed corpus; `review_import_cross_module` checks a type flowing across a module
import. The Nix expression generator now also produces `NSelect` via
`genNixSelect`, so selection paths are fuzzed throughout.

### `narsil-fixtures` (Fixtures.hs)

Integration tests against real-world scripts:

- **check-by-name.sh** -- nixpkgs script, verifies env var extraction and bare command detection
- **qemu-common.nix** -- verifies type inference and `rec`/`with` lint detection
- **kernel.nix** -- clean file, verifies zero violations
- **gpu-broker layout** -- verifies `_class` validation (valid and invalid cases)

### `narsil-more-fixtures` (MoreFixtures.hs)

- **nativelink integration** -- real integration test script, verifies heredoc detection and bare command flagging
- **isospin main** -- large Nix file, verifies `rec`/`with` lint, bash extraction (10+ scripts), specific bare command detection

### `narsil-flake-parts` (FlakePartsTest.hs)

- Flake-parts `flake.nix` parsing
- Bash script with `@shell@` substitution placeholders
- Module directory lint (observation mode)

### `narsil-oracle` (Oracle.hs)

The differential soundness oracle — the one property a type checker most needs:
"accept ⟹ the runtime type matches what we claimed." For each closed expression
in a fixed corpus it compares the inferred kind against the runtime kind reported
by `nix-instantiate --eval -E 'builtins.typeOf (EXPR)'`. The corpus spans
literals, arithmetic, string/path concat, comparison/equality, collections,
nested selection, lambdas/application, polymorphic and row-polymorphic builtins,
and expressions that should type-error at runtime.

Verdicts: `MISMATCH` (checker claimed a kind the runtime contradicts) and
`CHECKER-HANG` (inference didn't terminate within the timeout) are failures;
`AGREE` / `AGREE-REJECT` pass; `INCOMPLETE` (conservative rejection) and
`typed-but-noeval` (typed but the expression doesn't evaluate, e.g. `head []`)
are noted, not failed. The current run reports **41 agree / 0 failures** — the
soundness gate against `nix-instantiate builtins.typeOf`. The suite skips cleanly
(vacuous pass) when `nix-instantiate` is not on PATH, e.g. inside the sandboxed
flake check.

## Property test design

The property tests follow an adversarial philosophy:

1. **No tautologies** -- every property asserts something structural about successful results, not just "no exception." Tests that previously followed `Left _ -> True; Right _ -> True` have been replaced with labeled assertions on output structure.

2. **Generators produce hostile input** -- injection attempts, overflow integers, malformed expansions, path traversal, Unicode in variable names. Bash generators include conditionals (`if/then/fi`), loops (`for/do/done`), pipes, and subshells. Nix generators include list concat (`++`), attrset merge (`//`), nested let, and attribute selection (`NSelect`).

3. **Properties assert invariants** -- algebraic laws (unification reflexivity/symmetry, substitution composition, overlay monoid laws), structural properties (balanced JSON braces, non-empty facts), and correctness vectors (specific bash patterns produce specific facts).

4. **Test vectors pin known behavior** -- specific expansion parses, literal types, overflow handling, merge semantics.

5. **Order-independence** -- constraint solving is tested against reversed input to catch order-dependent bugs.

The `Adversarial.hs` module contains additional security-focused properties (injection blocking, store path traversal rejection, bounded resource tests) that run alongside the main property suite.

### `Psychotic.hs` — review-2 regression suite

`test/Psychotic.hs` holds the regression suite for the second-round adversarial
audit (`REVIEW-2.md`). Each finding gets at least one negative test (input that
previously crashed or accepted bad code) and at least one positive test (input
that still works correctly after the fix). The suite currently registers 22 tests
(`Psychotic.psychoticTests`) grouped by finding:

| Group | Tests | Subject |
|-------|-------|---------|
| C1    | 5     | `escapeForParamExpansion` is idempotent, neutralizes newlines and single quotes, safe defaults pass through, end-to-end injection neutralized through `parseScriptFile` → `emitConfigFunction` |
| C2    | 1     | Deep `NSelect` chain doesn't crash the depth analyzer |
| C3    | 4     | `analyzeDepth` rejects `NWith`/`NApp` bypass chains; shallow ASTs accepted; error names the constructor |
| C4    | 1     | `safeParseNixText` survives deeply nested lists |
| C6    | 1     | Local Dhall config still loads |
| S2    | 2     | Closed-set missing-with-default works; present-key works |
| S3    | 2     | Let-bound variable works; `envLenient = True` retains old behavior |
| S6    | 3     | `true + false` fails; `Int + Int`, `String + String` work |
| B1    | 1     | `combinedLintSafe` returns `LintOk` on clean input |
| Safety wrappers | 2 | `safeIO` catches exceptions; `safeReadFile` returns Left for a missing path |
