# Safety

`Narsil.Safety` is the single source of truth for the recursion-depth
limit, the parser exception wrappers, and the structured DoS-error type. Every
public entry point routes through it before doing any analysis.

## Why one module

Three prior incidents motivated centralising this:

1. **Depth-guard divergence.** Three independent magic `200` constants
   (`Check.hs`, `LintCombined.hs`, two copies in `Check.hs`) drifted such that
   one could be tightened without the others. Any future bump now happens in
   one place.
2. **Depth-guard bypass.** The `detectUnsupportedConstruct` walker only
   incremented depth inside matched constructors, so `NWith`/`NStr`/`NSynHole`
   chains evaded detection. `analyzeDepth` counts every `Fix` unwrap regardless
   of constructor.
3. **Parser stack overflow.** `try @IOException` catches IO errors but **not**
   `Control.Exception.StackOverflow` thrown by megaparsec on adversarially-nested
   input. `safeParseNixFile`/`safeParseNixText` use `try @SomeException` so the
   process survives.

## API

```haskell
maxRecursionDepth :: Int
maxRecursionDepth = 200

analyzeDepth :: NExprLoc -> Either DepthError ()
analyzeDepth = analyzeDepthWith maxRecursionDepth

safeParseNixFile :: FilePath -> IO (Either SafetyError NExprLoc)
safeParseNixText :: Text     -> IO (Either SafetyError NExprLoc)
safeReadFile     :: FilePath -> IO (Either SafetyError Text)
safeIO           :: IO a     -> IO (Either SafetyError a)

safeAnalyze :: NExprLoc -> Either SafetyError NExprLoc
```

`SafetyError` is a closed sum:

```haskell
data SafetyError
    = SafetyDepthExceeded !DepthError
    | SafetyParseFailed !Text
    | SafetyStackOverflow
    | SafetyInternalException !Text
    | SafetyIOError !Text
```

`renderSafetyError` produces a single-line diagnostic suitable for the CLI.

## Where it's wired

| Entry point         | Module                           | Notes                                     |
|---------------------|----------------------------------|-------------------------------------------|
| `check`             | `CLI.Check.checkFile`            | `analyzeDepth` precedes `inferExpr`       |
| `infer`             | `CLI.Dispatch.cmdInfer`          | via `withSafeNix`                         |
| `fmt`               | `CLI.Dispatch.cmdFmt`            | via `withSafeNix`                         |
| `scope*`            | `CLI.Dispatch.cmdScope*`         | via `withSafeNix`                         |
| `ci` walker         | `CLI.CI.walkDirectory`           | path-separator boundary; see review-2 C5  |
| LSP handlers        | `LSP.Handlers.lspSafeParse`      | catches parse+depth in one helper         |
| Module loader       | `Nix.Module.processFile`         | `safeParseNixFile`                        |
| Lint                | `Nix.LintCombined.combinedLintSafe` | `LintDepthExceeded` distinguishable    |

## Performance

`analyzeDepth` is cheap — roughly 1 ns per AST node (see
`cabal run narsil-bench` → `analyzeDepth` group). A 1000-element list takes
~2 μs to validate; a 150-deep app chain ~457 ns. Compared to the ~150 μs hnix
spends parsing a let-chain-50, the depth check is below the noise floor.
