<!--
  // hypermodern // haskell // narsil
  Adapted from the "Production Haskell" guide (gist 2f558f27) for this repo's
  GHC 9.12 / GHC2021 pin, two-space fourmolu, katip, and the nix-flake-check
  gate. Treat it as living: where we deviate, the deviation is documented here,
  not left implicit in the source.

  Companion: TYPOGRAPHY.md governs the visual layer — delimiters, banners,
  epigraph watermarks, 100-column discipline, comment capitalization.
-->

# `// hypermodern // haskell // narsil`

> Companion: **[TYPOGRAPHY.md](TYPOGRAPHY.md)** — the visual conventions (Unicode
> delimiters, 100-column banners, epigraph watermarks). This document is *what to
> write*; that one is *how it looks*.

## Why we do what we do

Production Haskell lives at the intersection of mathematical beauty and economic
reality. We write in a language that *could* express category theory and choose
to express a type checker for bash instead — not because we can't do the former,
but because shipping a tool people rely on is the harder, more honest problem.

This codebase is read far more often than it is written, and most of the reading
happens under pressure: an agent extending a subsystem it has never seen, a human
bisecting a regression at the end of a long day. **Every ambiguity compounds.** So
we optimize relentlessly for *disambiguation* — the reader should never have to
hold more than one hypothesis about what a line means.

```haskell
-- costs 0.1s to write, 10 minutes to debug
process e = if p e > 0 then go e else stop

-- costs 0.2s to write, saves the 10 minutes every time it's read
processEdgeConfiguration :: EdgeConfiguration -> IO ProcessResult
processEdgeConfiguration edgeConfig
  | edgeConfigPort edgeConfig > 0 = processValidConfiguration edgeConfig
  | otherwise = returnInvalidPortError
```

Beauty here is not cleverness. It is the property that a tired reader arrives at
the correct understanding on the first pass.

## The binding law: guards and equations over `case`

This is the one rule that overrides taste, habit, and convenience. **If a `case`
can be written another way, it is written another way.** It is enforced
mechanically by **`straylint`** — our own linter, built on GHC's real parser
(`ghc-lib-parser`, so it sees Template Haskell correctly where tree-sitter /
ast-grep do not). `straylint` is the seed of a larger analyzer, the narsil
of Haskell; today it carries one rule, this one. The `narsil:case-ban` flake
check runs it `--strict` over the set of modules already swept clean, so they
cannot regress; the allowlist grows until it covers the tree. Write as if it is
already enforced everywhere — because on the cleaned files it is.

`case` is not banned because it is wrong — it is demoted because nearly every
`case` is a flatter, more honest construct wearing a disguise. The alternatives
are almost always clearer:

1. **Function-clause equations** — when you `case` on an argument, match in the
   head instead. Each clause is independently readable; the compiler checks
   totality per equation.

   ```haskell
   -- NO: case on the argument
   classify x = case x of
     Flake -> "flake"
     Package -> "package"
     _ -> "other"

   -- YES: equations
   classify :: ModuleKind -> Text
   classify Flake = "flake"
   classify Package = "package"
   classify _ = "other"
   ```

2. **Pattern guards** — when the scrutinee is a computed `Maybe`/`Either`/tuple,
   bind it in a guard. This flattens the staircase the original guide rightly
   calls a "maintenance liability."

   ```haskell
   -- NO: nested case staircase
   route request = case validate request of
     Nothing -> handleInvalid
     Just valid -> case findRoute valid of
       Nothing -> handleNoRoute
       Just r -> execute r valid

   -- YES: pattern guards in a where-clause
   route :: Request -> Response
   route request = dispatch
    where
     dispatch
       | Nothing <- validate request = handleInvalid
       | Just valid <- validate request, Nothing <- findRoute valid = handleNoRoute
       | Just valid <- validate request, Just r <- findRoute valid = execute r valid
   ```

   When the same scrutinee is needed by several guards, bind it once in `where`
   and guard on the binding — clarity first, the compiler removes the re-eval.

3. **`maybe` / `either` / `fromMaybe`** — for the two-armed cases, the
   eliminator names the intent better than `case` ever will.

   ```haskell
   -- NO
   greeting = case lookup name table of
     Just v -> v
     Nothing -> "stranger"

   -- YES
   greeting = fromMaybe "stranger" (lookup name table)
   ```

`\case` (LambdaCase) is `case` with the scrutinee hidden — it is *more* opaque,
not less, and so it is held to the same rule: prefer a named function with
equations. A bare `\case` lambda passed to `maybe`/`either`/`foldr` should become
a named, type-signed helper.

**The narrow exception.** A `case` is acceptable only when there is genuinely no
equation or eliminator form — typically a small, single-use, local match on a
value produced mid-expression (often monadically bound) where lifting it to a
named `where`-helper costs more in naming and non-locality than it buys in
flatness. When you reach for it, mark the line with **`CASE-OK`** (and a reason):
`straylint` honours that marker as the sanctioned escape, so the survivors stay
counted, visible, and few — not forbidden and smuggled.

## Comments: expository, almost literate

Code says *what*. Comments say *why*, and *why not the obvious alternative*. We
write them as if narrating to a competent reader who lacks our context — because
that reader is the next agent, and the next incident.

- **Banner headers** orient the reader entering a module:

  ```haskell
  -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  --                                                       // layout convention
  -- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ```

  Section rules (`-- ═══`) divide a module into the few movements it actually has.

- **`n.b.` notes** carry the non-obvious: an invariant, a subtlety, the reason a
  tempting simplification is wrong. These are the highest-value comments in the
  tree; the rewrite should *add* them wherever the code earns one.

  ```haskell
  -- n.b. the boundary check must include the path separator: without it,
  -- `/home/u/proj` is treated as a prefix of `/home/u/proj-evil`.
  ```

- **Provenance markers** distinguish reasoning from convention. A line that
  encodes domain knowledge an agent could not have inferred gets attributed:

  ```haskell
  -- human: nasdaq sends duplicate timestamps during market open, dedupe here
  ```

- **Haddock (`-- |`, `{- | … -}`)** on every exported binding. fourmolu is pinned
  to `haddock-style: multi-line`; a one-line `-- |` is fine, a paragraph earns the
  block form. The signature plus the Haddock should let a reader use the function
  without reading its body.

The bar: a newcomer should be able to read a module top to bottom and come away
with the *design*, not just the mechanics.

## Language extensions: a hierarchy of trust (GHC 9.12 / `GHC2021`)

Our first-party `default-language` is `GHC2021`, which already folds in the
former everyday extensions (`ScopedTypeVariables`, `DeriveGeneric`,
`StandaloneDeriving`, `BangPatterns`, `InstanceSigs`, and friends) — so they need
no pragma and earn no discussion. The vendored `nixfmt` is `Haskell2010` and
exempt from everything here. We pin GHC 9.12; `GHC2024` is available but we have
not opted in, precisely because some of what it standardizes (`LambdaCase`) cuts
against the binding law above.

### Green — use freely

```haskell
{-# LANGUAGE OverloadedStrings #-}     -- Text everywhere; our most-used pragma
{-# LANGUAGE RecordWildCards #-}       -- tasteful destructuring at use sites
{-# LANGUAGE NamedFieldPuns #-}        -- clear intent without the {..}
{-# LANGUAGE DerivingStrategies #-}    -- always say HOW you derive
{-# LANGUAGE StrictData #-}            -- default-strict fields; we mark ! anyway
{-# LANGUAGE NumericUnderscores #-}    -- 1_000_000
```

### Yellow — use with purpose

```haskell
{-# LANGUAGE TemplateHaskell #-}       -- katip's $(logTM) only; measure build cost
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for the monad stack newtypes
{-# LANGUAGE DuplicateRecordFields #-} -- when domain records genuinely collide
{-# LANGUAGE TypeApplications #-}      -- to disambiguate, not to show off
{-# LANGUAGE PatternSynonyms #-}       -- only where it makes a type read better
{-# LANGUAGE DataKinds #-}             -- ONLY to consume lsp-types' promoted symbols
```

`TemplateHaskell` is yellow, not green: it costs build time and stage
restrictions. We accept it for exactly one thing — katip's compile-time log
location splice (`$(logTM)`) — and we do not reach for it elsewhere.

`DataKinds` is yellow, not red, for one reason: it is *forced by a dependency,
not chosen for type-level programming of our own*. `lsp-types` exposes its typed
handler API through promoted constructors — every handler signature names a
`'Method_*` symbol (`TRequestMessage 'Method_TextDocumentHover`,
`TNotificationMessage 'Method_Initialized`, …), and those leading-tick promoted
data constructors require `DataKinds` to write. No rewrite keeps the typed LSP
API without it. Its single foothold is `Narsil.LSP.Handlers` (and that
subtree); it stays confined there. We do **not** reach for `DataKinds` to do
type-level programming of our own — *that* remains red, below.

### Red — justify your existence (and prefer to remove)

```haskell
{-# LANGUAGE LambdaCase #-}            -- it is `case` in a trenchcoat; see the law
{-# LANGUAGE DataKinds #-}             -- for type-level programming of our OWN (see yellow)
{-# LANGUAGE NondecreasingIndentation #-} -- a layout escape hatch; refactor instead
{-# LANGUAGE UndecidableInstances #-}  -- usually the wrong problem
{-# LANGUAGE ImplicitParams #-}        -- a debugging nightmare
```

`LambdaCase` is red *here* specifically because of the binding law; it carries no
live use in the case-ban-covered tree (`lib/`, `app/`, `straylint/`), and any new
one is a rewrite target. `DataKinds` is split: **red** when *we* author type-level
machinery, **yellow** (above) only when consuming `lsp-types`' promoted method
symbols — the lone sanctioned foothold. `NondecreasingIndentation` and the other
two carry no foothold in the tree today; keep it that way.

## Control flow: flat is a feature

Deep nesting is a maintenance liability, not elegance. Every level of indentation
is somewhere a merge conflicts, a reviewer argues about whitespace, and a layout
rule bites. The production pattern: a **small `do` for sequencing**, a
**`where`-clause of guarded equations for logic**.

```haskell
handleWebRequest :: Request -> AppM Response
handleWebRequest request = do
  startTime <- getCurrentTime
  validated <- validateOrReject request
  result <- processRequest validated
  recordMetrics startTime result
  pure result
 where
  validateOrReject req
    | not (validMethod req) = throwError InvalidMethod
    | not (validHeaders req) = throwError InvalidHeaders
    | otherwise = pure req

  processRequest req
    | isHealthCheck req = pure healthCheckResponse
    | needsAuth req && not (hasValidAuth req) = throwError Unauthorized
    | otherwise = routeToHandler req
```

Note the two-space `where` aligned one stop under the body — that is what our
pinned fourmolu produces; do not fight it by hand.

## Naming: the three-character rule

If an identifier is three characters or fewer it is probably too short for code
that outlives the function it sits in.

```haskell
-- NO: abbreviations multiply hypotheses
cfg <- loadCfg
conn <- mkConn cfg

-- YES: full words tell the story
configuration <- loadServerConfiguration
connection <- createDatabaseConnection configuration
```

**Sanctioned short names**, only in local scope where the type removes all doubt:
`xs`/`ys` (lists in pure folds), `m`/`n` (indices), `k`/`v` (map key/value),
`f`/`g` (functions in higher-order contexts), `t` (a `NixType` in a tight unify
clause). Everywhere else, spell it.

**Acronyms keep their capitalization.** A well-known acronym is one word and
wears all caps wherever it appears — in module names (`Narsil.CLI`,
`Narsil.LSP`), type names, and identifiers (`parseJSON`, `lspSafeParse`,
the `LSP` in a constructor). We do **not** title-case them to `Cli` / `Lsp` /
`Json`: the acronym is the word, and lowercasing its tail buries the very
signal that makes it readable. (This is the one place the unabbreviated-naming
rule yields — `LSP` is *more* legible than `LanguageServerProtocol`.)

## Make invalid states unrepresentable

Push correctness into types so the wrong program does not compile.

```haskell
-- NO: a soup of independent booleans permits impossible combinations
data Connection = Connection { isConnected :: Bool, isAuthed :: Bool, hasError :: Bool }

-- YES: a sum type that cannot be in two states at once
data ConnectionState
  = Disconnected
  | Connecting ConnectingInfo
  | Connected ConnectionInfo
  | Errored ErrorInfo
```

Newtypes guard domain boundaries (`newtype CustomerId = CustomerId Int64`), units
(`newtype Milliseconds = …`), and fallible construction (`mkEmail :: Text ->
Either ValidationError Email`). Start with a type alias; upgrade to a newtype the
moment two things get mixed up. With `-O2` the wrapper is free.

## Effects are explicit

`IO`, `STM`, and pure must be obvious from the signature. STM earns its keep where
state is shared: transactions compose and retry without a lock in sight (no `IO`
inside, and watch for starvation on long transactions). Pure stays pure; the
inference core (`Narsil.Nix.Inference`) is a `State`+`Except` monad with no
`IO`, and that is load-bearing — it is why the oracle can replay it.

## Compiler warnings: your automated colleague

Strict warnings catch more bugs than they cost in refactoring. The library and
every first-party component build under, and `-Werror`-fail on:

```
-Wall -Werror -Wcompat -Widentities
-Wincomplete-record-updates -Wincomplete-uni-patterns
-Wmissing-export-lists -Wmissing-home-modules
-Wpartial-fields -Wredundant-constraints
```

The only exemption is the vendored `nixfmt` sub-library, which adds `-Wno-orphans`
because upstream's `Pretty` instances are orphans by design. `hlint` is advisory,
not a gate — do not let its suggestions override the rules in this document (it
will, for instance, suggest `case` rewrites we have already made better).

## Logging and diagnostics

Two distinct channels, never confused:

- **stdout is data.** Machine-readable output (schemas, facts, JSON) and nothing
  else. A diagnostic must never reach stdout. `tools/clicheck` guards this.
- **stderr is for humans.** Findings render as unified clippy-style diagnostics
  (`error[CODE]: summary`, a `-->` location, an optional source snippet with a
  caret), emitted through katip at their own severity with selective ANSI colour
  when stderr is a TTY. katip namespaces/contexts carry the structure; we do not
  hand-roll `printf` log lines.

## Testing: the soundness gate is non-negotiable

- **Property tests for invariants** (`test/Props.hs`): one property per behaviour,
  named for the thing it pins. Known-but-unfixed bugs live as `expectFailure`
  tripwires that flip green the moment the fix lands.
- **The differential oracle** (`test/Oracle.hs`) compares every inferred type
  against `nix-instantiate`'s `builtins.typeOf`. A MISMATCH is a real soundness
  bug, full stop. This is *the* gate; it is why the inference core stays pure.
- **CLI / layout guards** (`tools/clicheck`, `tools/layoutcheck`) run the real
  binary against fixtures.

Everything runs through **`nix flake check`** — the single disciplined runner.
If a check isn't wired into it, it atrophies; do not add a parallel manual path.

## Formatting

fourmolu is pinned by `fourmolu.yaml` (the full `--print-defaults` with
`indentation: 2`), so style is explicit and stable across fourmolu versions
rather than tracking shifting defaults. treefmt runs it; `vendor/` is excluded so
the vendored nixfmt stays byte-diffable against upstream. Note treefmt caches by
file hash — a config change needs `treefmt --no-cache` once.

Commit subjects follow `// narsil // imperative summary`.

## Performance: clear first, fast where it's measured

Write the obvious version, compile with `-O2`, and let GHC do the easy wins. Then
*measure* (`bench/`) and optimize the hot path with data in hand — strictness
(`!`, `BangPatterns`), unboxed vectors, fewer allocations — never on a hunch.
Clarity is the default; speed is a justified, benchmarked exception. (This is the
on-ramp to the performance pass that follows the style pass.)

## The vibe test

Good code here passes all of these:

- Could you debug it during an incident without `ghci`?
- Could the next contributor — human or agent — extend it without breaking an
  invariant?
- Do the types prevent tomorrow's bug?
- Is every abbreviation worth the confusion it buys?
- Did you reach for `case` where an equation or a guard would have been clearer?
- Will it still make sense after a hundred hands have touched it?

## Required reading

- Wadler & Blott, *Making Ad-Hoc Polymorphism Less Ad Hoc* (1989)
- Wadler, *Monads for Functional Programming* (1995)
- Harris et al., *Composable Memory Transactions* (2005)
- ekmett/`lens`, haskell/`aeson`, simonmar/`async` — production-grade exemplars
- Marlow, *Parallel and Concurrent Programming in Haskell*

---

We are not the Haskell you learned in school. We are what happens when those ideas
have to hold up under a regression at 2am — and stay beautiful while they do.
