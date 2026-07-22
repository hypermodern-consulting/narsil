# Architecture

narsil is a Haskell static analysis tool that provides compile-time type checking for Nix expressions and their embedded bash scripts. The system comprises two independent analysis pipelines — one for bash, one for Nix — unified under a shared CLI, LSP server, and configuration framework.

## Table of Contents

- [System Overview](#system-overview)
- [Pipeline A: Bash Analysis](#pipeline-a-bash-analysis)
- [Pipeline B: Nix Type Inference](#pipeline-b-nix-type-inference)
- [Nix-to-Bash Bridge](#nix-to-bash-bridge)
- [Module Graph Builder](#module-graph-builder)
- [Scope Graphs](#scope-graphs)
- [LSP Server](#lsp-server)
- [Formatter (vendored nixfmt)](#formatter-vendored-nixfmt)
- [Type-Annotated Output](#type-annotated-output)
- [Layout Conventions](#layout-conventions)
- [Effect Algebra](#effect-algebra)
- [Configuration System](#configuration-system)
- [CLI Layer](#cli-layer)
- [Module Dependency Map](#module-dependency-map)
- [Build and Test Infrastructure](#build-and-test-infrastructure)

---

## System Overview

The system processes two distinct languages — Nix expressions and bash scripts — through separate type-inference pipelines. These pipelines converge in three places: (1) the CLI dispatch layer, (2) the LSP diagnostics engine, and (3) the Nix-to-bash extraction bridge in `Narsil.Syntax.Parse`.

```
                        ┌─────────────────────┐
                        │     CLI Dispatch    │
                        │  Narsil.CLI.*   │
                        └───────┬──────┬──────┘
                                │      │
                   ┌────────────┘      └────────────┐
                   ▼                                 ▼
┌──────────────────────────┐       ┌──────────────────────────┐
│   Pipeline B: Nix        │       │   Pipeline A: Bash       │
│   ┌──────────────────┐   │       │   ┌──────────────────┐   │
│   │ hnix → Infer →   │   │       │   │ ShellCheck →     │   │
│   │ Lint → Scope →   │◀──┼───────┼───│ Facts → Unify →  │   │
│   │ Module → Layout  │   │bridge │   │ Schema           │   │
│   └──────────────────┘   │       │   └──────────────────┘   │
│                          │       │                          │
│   ┌──────────────────┐   │       │                          │
│   │ Formatter / Infer│   │       │                          │
│   │ (nixfmt / annot) │   │       │                          │
│   └──────────────────┘   │       │                          │
└──────────────────────────┘       └──────────────────────────┘
                                │
                    ┌───────────┴───────────┐
                    │                       │
                    ▼                       ▼
          ┌─────────────────┐    ┌─────────────────┐
          │   LSP Server    │    │  Config (Dhall) │
          │ 15 handlers     │    │ Profiles + Rules│
          └─────────────────┘    └─────────────────┘
```

The cross-cutting bridge from Nix to bash works by extracting bash source text from `writeShellScript` / `writeShellScriptBin` / `writeShellApplication` calls found in parsed Nix files, then feeding that text through the entire bash pipeline — complete with interpolation tracking.

---

## Pipeline A: Bash Analysis

**Goal:** Given a bash script (standalone or embedded in Nix), produce a typed schema describing its environment variables, configuration paths, and command usage. Detect forbidden constructs (eval, backticks, heredocs).

### Step 1: Parsing — `Narsil.Bash.Parse`

```haskell
parseBash :: Text -> Either Text BashAST
parseBashWithFilename :: FilePath -> Text -> Either Text BashAST
```

Wraps [ShellCheck](https://github.com/koalaman/shellcheck) (`ShellCheck >= 0.9`) via its library API. Produces a `BashAST` consisting of ShellCheck's token tree augmented with a position map (`[SourcePos]`). The parse is lossless relative to ShellCheck's own parser — all token types, quoting modes, and redirections are preserved.

Error handling: parse failures return `Left Text` with ShellCheck's error message. No partial recovery is attempted — a syntactically invalid script aborts the entire pipeline for that script.

### Step 2: Fact Extraction — `Narsil.Bash.Facts`

```haskell
extractFacts :: BashAST -> [Fact]
```

Walks the ShellCheck token tree and pattern-matches against bash idioms to produce `Fact` values. Each `Fact` carries a `Span` (source location) for downstream diagnostics.

**Fact types** (from `Narsil.Bash.Types`):

| Constructor | Meaning | Example |
|---|---|---|
| `DefaultIs v lit sp` | Variable has a default literal | `PORT=${{PORT:-8080}}` → `DefaultIs "PORT" (LitInt 8080)` |
| `DefaultFrom v other sp` | Default is another variable | `HOST=${HOST:-$FALLBACK_HOST}` |
| `Required v sp` | Unset variable reference without default | `$PORT` with no `:-` |
| `AssignFrom v other sp` | Variable assigned from another variable | `X=$Y` |
| `AssignLit v lit sp` | Variable assigned from a literal | `PORT=8080` |
| `ConfigAssign path v q sp` | Variable assigned from a config path | `config.server.port=$PORT` |
| `ConfigLit path lit sp` | Config path set to a literal | `server.port=8080` |
| `ConfigTemplate path parts q sp` | Config path with template parts | `url = "https://${HOST}:${PORT}"` |
| `CmdArg cmd arg var sp` | Command argument references a variable | `curl "$URL"` |
| `UsesStorePath path sp` | A known store path is referenced | `/nix/store/...-curl/bin/curl` |
| `BareCommand cmd sp` | External command without a store path | `wget`, `ping` |
| `DynamicCommand var sp` | Command selected via variable | `$($CMD)`, `"$WRAPPER"` |

**Key design decisions:**
- Quoting is tracked via `Quoted` / `Unquoted` annotations on config-related facts.
- Config paths are nested lists (`[Text]`) — `config.services.nginx.port` becomes `["services", "nginx", "port"]`.
- `BareCommand` and `DynamicCommand` are policy violations (not type errors) — they indicate commands that cannot be statically verified.

### Step 3: Pattern Recognition — `Narsil.Bash.Patterns`

```haskell
-- Internal helpers for matching bash parameter expansion patterns
-- Handles: ${VAR:-default}, ${VAR:=default}, ${VAR:?error},
--          config.x.y=$VAR, ${#VAR}, and similar idioms
```

Provides the low-level pattern matching used by `Narsil.Bash.Facts`. Recognizes parameter expansion syntax (`:-`, `:=`, `:+`, `:?`, `#`, `%`), array reference patterns (`${arr[@]}`, `${arr[*]}`), and indirect variable references (`${!name}`).

### Step 4: Builtin Database — `Narsil.Bash.Builtins`

A typed flag database for 21 shell builtins (`echo`, `printf`, `test`, `read`, `export`, `source`, `cd`, `exit`, `return`, `exec`, etc.) and common external commands (`curl`, `jq`, `nix`, `cat`, `mkdir`, `chmod`). Each entry records:
- The command name and classification (builtin vs external vs unknown)
- Expected argument flags and their types
- Flag relationships (mutually exclusive flags, required argument flags)
- Whether the command accepts store-path arguments

This database allows the fact extractor to distinguish between bare commands that are shell builtins (safe) and bare external commands (policy violation).

### Step 5: Constraint Generation — `Narsil.Inference.Nix.Constraint`

```haskell
factsToConstraints :: [Fact] -> [Constraint]
```

Converts facts into `Constraint` values (type equalities). The constraint type is:

```haskell
data Constraint = Type :~: Type
```

**Constraint generation rules:**

| Fact | Generated Constraints |
|---|---|
| `DefaultIs v lit` | `TVar v :~: literalType lit` |
| `AssignLit v lit` | `TVar v :~: literalType lit` |
| `AssignFrom v other` | `TVar v :~: TVar other` |
| `DefaultFrom v other` | `TVar v :~: TVar other` |
| `ConfigAssign path v q` | `TVar v :~: cfgType(q)` (string if unquoted, config context if quoted) |
| `ConfigLit path lit` | No constraint on variables; records the config path literal type |
| `CmdArg cmd arg var` | `TVar var :~: expected type from Builtins DB` |
| `UsesStorePath` | No constraint (informational) |
| `BareCommand` / `DynamicCommand` | No constraint (policy violations reported separately) |

### Step 6: Unification — `Narsil.Inference.Nix.Unify`

```haskell
solve :: [Constraint] -> Either UnifyError Subst
```

First-order unification over simple types (`TInt`, `TString`, `TBool`, `TPath`, `TNumeric`, `TVar`). Implements:
- Occurs check (prevents infinite types)
- Compositional substitution via `composeSubst`
- Union-find style variable binding (a variable bound to another variable chains through the substitution)

The substitution (`Subst = Map TypeVar Type`) maps type variables to concrete types. After unification succeeds, all facts have resolved types.

### Step 7: Schema Construction — `Narsil.Inference.Bash.Schema`

```haskell
buildSchema :: [Fact] -> Subst -> Schema
validateConfigPaths :: [Fact] -> Either Text ()
```

Assembles the final `Schema` from facts and the resolved substitution. The `Schema` type aggregates all information needed for code generation and policy checking:

```haskell
data Schema = Schema
    { schemaEnv            :: Map Text EnvSpec       -- env var → type + required/default
    , schemaConfig         :: Map ConfigPath ConfigSpec  -- config path → type + source
    , schemaCommands       :: [CommandSpec]          -- verified commands
    , schemaStorePaths     :: Set StorePath          -- all referenced store paths
    , schemaBareCommands   :: [Text]                 -- policy violations
    , schemaDynamicCommands :: [Text]                -- unanalyzable commands
    , schemaDefaultedVars  :: [Text]                 -- vars with safe defaults
    }
```

`mergeSchemas` provides composition of schemas (e.g., for when a Nix file contains multiple shell scripts). Field-specific merge functions (`mergeEnvSpec`, `mergeConfigSpec`) handle deduplication.

`validateConfigPaths` checks that config path references are well-formed (no empty segments, no leading/trailing dots).

### Step 8: Config Emission — `Narsil.Emit.Config`

```haskell
emitConfigFunction :: Schema -> Text
```

Generates a self-contained bash function (`emit_config()`) that, when sourced, validates environment variables against the schema at runtime. The generated code checks:
- Required variables are set (errors if not)
- Type coercions are applied (string → int, etc.)
- Config paths are populated from variables

### Step 9: Bash Linting — `Narsil.Lint.Forbidden`

```haskell
findViolations :: BashAST -> [Violation]
formatViolationsAt :: Text -> [Violation] -> Text
```

Detects forbidden shell constructs (regardless of typing):
- `VHeredoc` — heredoc (`<<`) in inline bash
- `VHereString` — here-string (`<<<`) in inline bash
- `VEval` — `eval` usage
- `VBacktick` — backtick command substitution

Each violation maps to an `ALEPH-Bxxx` error code (B001–B004). Violations are independent of the type inference pipeline and can be checked even if type inference fails.

---

## Pipeline B: Nix Type Inference

**Goal:** Given a Nix expression, infer the type of every binding (name → `NixType`) using full Hindley-Milner type inference with row polymorphism and let-polymorphism.

### Type System — `Narsil.Inference.Nix.Type`

```haskell
data NixType
    = TVar TypeVar        -- type variable (fresh, quantified)
    | TInt | TFloat | TBool | TString | TPath | TNull | TDerivation
    | TStrLit Text        -- literal string (singleton type for interpolation tracking)
    | TFun NixType NixType    -- function type
    | TList NixType           -- list type
    | TRec (Map Text (NixType, Bool)) RowTail  -- record: known fields + row tail
    | TUnion [NixType]   -- union (sum) type
    | TAny               -- top type (dynamic/unknown)
    deriving (Eq, Ord, Show)

data RowTail = RClosed | ROpen TypeVar  -- closed record, or open with a row variable

data Scheme = Forall [TypeVar] NixType  -- polymorphic type scheme
```

`TAttrs` and `TAttrsOpen` survive as bidirectional **pattern synonyms** over `TRec` (`TAttrs m = TRec m RClosed`; `TAttrsOpen m` matches any `TRec m (ROpen _)`), so existing call sites and the rest of this document continue to read in those terms.

**Key design decisions:**
- **Row polymorphism** via `TRec fields RowTail`. A `RClosed` tail (the `TAttrs` view) is an exact record that unifies only when all keys match; an `ROpen r` tail (the `TAttrsOpen` view) carries a row **variable** `r` standing for "at least these fields plus whatever `r` resolves to", letting open records accumulate fields across unifications. This models Nix's structural typing where attribute sets passed to functions may carry extra fields. (Row-variable lacks-constraints — labels a row must NOT gain — live in a side store in the inference state; see [Nix type inference](nix-inference.md) for the full engine.)
- **Required field tracking**: the `Bool` flag in record fields tracks whether a field is required (`False`) or optional (`True`). Lambda patterns with defaults produce optional fields.
- **Union types** via `TUnion` model sum types (e.g., `toString` accepts `Int | Float | Bool | Path | String`).
- **String literals** (`TStrLit`) distinguish literal strings from general `TString` — useful for tracking known strings vs interpolated ones, especially for bash extraction.
- **`__functor` support** — when a function type is unified against an attribute set type, the system checks for a `__functor` field in the record and follows the protocol chain. This models Nix's "callable attribute sets."

### Type Substitution — `Narsil.Inference.Nix.Type`

```haskell
type Subst = Map TypeVar NixType

emptySubst :: Subst
singleSubst :: TypeVar -> NixType -> Subst
composeSubst :: Subst -> Subst -> Subst
applySubst :: Subst -> NixType -> NixType
applySubstScheme :: Subst -> Scheme -> Scheme
freeTypeVars :: NixType -> Set TypeVar
freeTypeVarsScheme :: Scheme -> Set TypeVar
```

Standard environment-based substitution with occurs check. Composition is left-biased: `composeSubst s1 s2` applies `s1` to the values in `s2`, then unions — effectively `s1 ∘ s2`.

### Inference Engine — `Narsil.Inference.Nix`

```haskell
inferExpr :: NExprLoc -> Either Text (NixType, [Binding])
inferExprWithEnv :: TypeEnv -> NExprLoc -> Either Text (NixType, [Binding])
inferFile :: FilePath -> IO (Either Text InferResult)
```

**Inference monad:** `Infer a = ExceptT Text (State InferState) a`

State carries:
- `inferSupply` — fresh type variable counter (monotonically increasing)
- `inferSubst` — current substitution
- `inferBinds` — accumulated `[Binding]` (name + type + span) for output
- `inferSpan` — current source location for error reporting
- `inferWithMemo` — cache for `with` scope field lookups (avoids re-unification)

**How each Nix AST node is handled:**

| AST Node | Handling |
|---|---|
| `NConstant atom` | Maps `NAtom` → `NixType` via `atomType` |
| `NStr str` | `DoubleQuoted [Plain t]` → `TStrLit t`; others → `TString` |
| `NLiteralPath` / `NEnvPath` | → `TPath` |
| `NSym name` | Lookup in `TypeEnv`, then `with` scope, then fresh type variable |
| `NList elems` | Infer first element, merge remaining elements against it via `mergeTypes`; empty list → fresh var |
| `NSet rec bindings` | Infer all bindings (recursive or non-recursive), produce `TAttrs` |
| `NLet bindings body` | SCC-based dependency analysis, let-generalization, infer body in extended env |
| `NIf cond then e` | `cond` must be `TBool`; `mergeTypes` over branches (produces union if needed) |
| `NWith scope body` | Infer scope as TAttrs, set `envWith`, infer body with memoised field lookups |
| `NAssert cond body` | `cond` must be `TBool`, then infer body |
| `NAbs params body` | Fresh var per parameter, infer body in extended env, produce `TFun` |
| `NApp func arg` | Unify func type as `TFun arg result`, return result; intercepts `import ./path` for cross-module types |
| `NSelect _ base attr` | Infer base, look up field in attr type; dynamic key → fresh var |
| `NHasAttr base path` | → `TBool` (presence not tracked at type level) |
| `NUnary op e` | Negation: arg must be `TInt`; Not: arg must be `TBool` |
| `NBinary op l r` | Operator-specific type constraints (see table below) |
| `NSynHole _` | → fresh type variable |

**Binary operator type constraints:**

| Operator | Left | Right | Result |
|---|---|---|---|
| `==`, `!=` | Any | Same as left | `TBool` |
| `<`, `<=`, `>`, `>=` | `TInt` | `TInt` | `TBool` |
| `&&`, `||`, `->` | `TBool` | `TBool` | `TBool` |
| `+` (nix plus is overloaded) | Any | Same as left | Same as left |
| `-`, `*`, `/` | `TInt` | `TInt` | `TInt` |
| `++` | `TList a` | `TList a` | `TList a` |
| `//` | `TAttrs l` (or open) | `TAttrs r` (or open) | Merged attrs (right wins) |

**Let-polymorphism (HM generalization):**

`inferLet` performs SCC-based dependency analysis on `let` bindings via `stronglyConnComp`:
1. Parse each binding into `(name, expr, span)` tuples.
2. Build dependency edges via `collectFreeVars`.
3. Run SCC to identify acyclic and mutually-recursive groups.
4. For each group: allocate fresh type variables, add to env, infer all expressions, unify, check for infinite types.
5. **Generalize** each binding's type: `freeTypeVars(type) \ freeTypeVars(env)` are quantified into a `Scheme`, giving let-polymorphism (each use-site gets independent instantiation).

**`with` scope resolution:**

When a symbol lookup fails in the lexical environment, the system checks `envWith` (the current `with` scope type). It constrains the scope's field to the lookup's result type, memoises the result, and caches it for the duration of the `with` body. This prevents repeated unification of the same field and is essential for large `with pkgs; ...` bodies.

**Cross-module type propagation (`import` interception):**

When `inferAppWithImport` detects an `import ./path` application whose path matches a previously-inferred module in `envImportTypes`, it uses the cached type directly instead of inferring the import afresh. This enables full-program type inference where module A importing module B sees B's inferred type.

**Builtin environment (~40 signatures):**

The builtin environment defines types for all standard Nix primitives: string/path conversions (`toString`, `baseNameOf`, `dirOf`), list operations (`head`, `tail`, `map`, `filter`, `foldl'`), attrset introspection (`attrNames`, `attrValues`, `hasAttr`, `getAttr`), type predicates (`isNull`, `isInt`, `isString`, etc.), arithmetic (`add`, `sub`, `mul`, `div`), file I/O (`readFile`, `import`, `toPath`), control flow (`throw`, `abort`, `trace`, `tryEval`), and `derivation` (typed as taking `TAttrsOpen` → `TDerivation`).

### Nix Linting — `Narsil.Lint.Nix`

```haskell
findNixViolations :: NExprLoc -> [NixViolation]
```

Detects problematic Nix patterns:
- `VWith` (ALEPH-N001) — `with lib;` usage (prefer explicit bindings)
- `VRec` (ALEPH-N002) — `rec { ... }` usage
- `VSubstituteAll` (ALEPH-N005) — `builtins.substituteAll`
- `VRawMkDerivation` (ALEPH-N006) — raw `builtins.derivation` calls (prefer `mkDerivation`)
- `VRawRunCommand` (ALEPH-N007) — raw `runCommand` calls
- `VRawWriteShellApplication` (ALEPH-N008) — raw `writeShellApplication`
- `VWriteShellScript` (ALEPH-N011) — `writeShellScript` (prefer `writeShellApplication`)
- `VLongInlineString n` (ALEPH-N012) — inline strings exceeding a length threshold

### Combined Linting — `Narsil.Lint.Combined`

```haskell
data LintBundle = LintBundle
    { lbNix    :: [NixViolation]
    , lbDeriv  :: [DerivViolation]
    , lbPattern :: [PatternViolation]
    }

combinedLint :: FilePath -> NExprLoc -> LintBundle
```

Runs all lint passes in one call. Used by the CLI `check` command and the LSP diagnostics engine.

### Derivation Linting — `Narsil.Lint.Derivation`

```haskell
findDerivViolations :: FilePath -> NExprLoc -> [DerivViolation]
```

Checks `mkDerivation` calls for:
- `VMissingMeta` — derivation without `meta` attribute
- `VMissingDescription` — `meta` set without `description` key

### Pattern Linting — `Narsil.Lint.Patterns`

```haskell
findPatternViolations :: NExprLoc -> [PatternViolation]
```

Checks for anti-patterns in Nix code:
- `VOrNullFallback` — `or null` pattern (use `if foo != null then foo else default`)
- `VAttrTranslation` — `lib.attrsets.translateAttrs` outside of prelude code

### Package Directory Linting — `Narsil.Lint.Packages`

```haskell
checkPackageDirs :: [FilePath] -> IO [PackageViolation]
```

Ensures every directory under a packages root contains a `default.nix` file (ALEPH-P001).

### Nix Parse / Bash Extraction — `Narsil.Syntax.Parse`

See [Nix-to-Bash Bridge](#nix-to-bash-bridge) below.

### Module Kind — `Narsil.Layout.ModuleKind`

```haskell
data ModuleKind = Flake | FlakeModule | NixOSModule | HomeModule
                | DarwinModule | Package | Overlay | Library | Shell | Unknown
```

Classification enum used by both the layout convention system and the module graph builder.

### Nix Naming — `Narsil.Layout.Naming`

Validation utilities for naming conventions (kebab-case, snake_case, camelCase, PascalCase) used by the layout convention system.

---

## Nix-to-Bash Bridge

**Module:** `Narsil.Syntax.Parse`

The bridge extracts bash source text from Nix files and feeds it through Pipeline A. It recognizes the following Nix function calls as shell-script producers:

- `writeShellScript name text` (positional)
- `writeShellScriptBin name text` (positional)
- `writeScript name text` (positional)
- `writeScriptBin name text` (positional)
- `writeShellApplication { name = "..."; text = ''...''; }` (record argument)

### Extraction Algorithm

```
Input: Nix file path
  1. Parse via hnix → NExprLoc (annotated AST)
  2. Walk AST looking for application nodes matching the known function names
  3. For positional forms: extract the name argument (string literal) and body argument
  4. For writeShellApplication: decompose the record to find `name` and `text` fields
  5. For each body string:
     a. Split into [Antiquoted Text NExprLoc] parts
     b. Replace Antiquoted expressions with stable placeholders:
        - Store-path interpolations → /nix/store/__nix_compile_interp_N__
        - Non-store-path interpolations → @__nix_compile_interp_N__@
     c. Record each Interpolation with its source expression and isStorePath flag
     d. Emit BashScript { name, content (bash text), interpolations, span }
  6. Return [BashScript]
```

**Interpolation tracking** is critical: it ensures that Nix-level interpolations like `${pkgs.curl}` are recognized as store paths in the bash analysis, while `${config.port}` is flagged as potentially dynamic. The placeholder injection preserves the bash syntactic structure so that ShellCheck's parser doesn't choke.

**Store-path heuristic** (`isStorePathExpr`): an expression is considered a store-path reference if it accesses `pkgs.<name>`, `lib.<name>`, or follows a variable named `pkgs*` or `*Pkg`/`*Package`.

---

## Module Graph Builder

**Module:** `Narsil.Layout.Graph`

The module graph builder analyzes multi-file Nix projects by walking `import` statements from a root file (typically `flake.nix`), constructing a dependency graph with topological ordering, and running type inference across all modules with cross-module type propagation.

### Data Model

```haskell
data Module = Module
    { modPath    :: FilePath         -- canonicalized path
    , modExpr    :: NExprLoc         -- parsed AST
    , modType    :: NixType          -- locally inferred type
    , modImports :: [Import]         -- imports found in this module
    }

data Import = Import
    { impPath    :: FilePath         -- resolved (relative to importing dir)
    , impRawPath :: Text             -- as written in source
    , impArgs    :: Maybe NExprLoc   -- extra args to import (if any)
    , impSpan    :: Span             -- source location
    }

data ModuleGraph = ModuleGraph
    { mgModules        :: Map FilePath Module     -- all loaded modules
    , mgRoot           :: FilePath                -- canonicalized root
    , mgOrder          :: [FilePath]              -- topological order
    , mgFailures       :: [ParseFailure]          -- files that couldn't parse
    , mgLintFailures   :: [LintFailure]           -- lint violations per file
    , mgLayoutFailures :: [LayoutFailure]         -- layout violations per file
    , mgModuleTypes    :: Map FilePath NixType    -- cross-module inferred types
    }
```

### Build Algorithm

```
buildModuleGraph :: FilePath -> IO (Either Text ModuleGraph)
  1. Canonicalize root path
  2. Recursively walk imports (guarded by visited set for cycle detection):
     a. Parse file with hnix
     b. findImports → extract import paths from AST
     c. Infer local type via inferExpr
     d. Run findNixViolations (lint)
     e. Run findLayoutViolations (layout checks)
     f. Recurse into each import (skipping paths outside root dir — vendored deps boundary)
  3. computeOrder → DFS-based topological sort from root
  4. inferModuleTypes → run type inference in topological order:
     - Dependencies are inferred first
     - Their types are fed into importers via extendImport
     - Both raw import paths (as written) and resolved/canonicalized paths are registered
  5. Return ModuleGraph with all modules, failures, and cross-module types
```

### Import Finding

`findImports` walks the Nix AST looking for `import ./path` calls. It handles:
- Direct `import ./path` — checks for `NSym "import"` at function position
- `builtins.import ./path` — checks for `NSelect` chains ending in `import`
- `import (./path + args)` — unwraps nested application chains
- Path extraction handles `NLiteralPath`, `NStr (DoubleQuoted [Plain t])`, and `NStr (Indented _ [Plain t])`

### Queries on the Graph

```haskell
moduleImports :: ModuleGraph -> FilePath -> [Import]       -- imports of a module
moduleDependents :: ModuleGraph -> FilePath -> [FilePath]  -- reverse dependency lookup
topologicalOrder :: ModuleGraph -> [FilePath]              -- pre-computed order
moduleTypes :: ModuleGraph -> Map FilePath NixType         -- inferred types
hasViolations :: ModuleGraph -> Bool                       -- any failures?
totalViolationCount :: ModuleGraph -> Int                  -- aggregate count
```

### Module System Extraction — `Narsil.Layout.ModuleSystem`

```haskell
extractOptions :: NExprLoc -> Map Text OptionInfo
```

Extracts NixOS module system options from parsed expressions. Each option has:
- `optPath` — option path (e.g., `services.nginx.virtualHosts`)
- `optType` — the NixOS option type (reconstructed from the expression)
- `optDescription` — documentation string
- `optDefault` — default value if specified

Used by the LSP hover handler to show option documentation.

---

## Scope Graphs

**Module:** `Narsil.Layout.Scope`

The scope graph subsystem follows the Visser-style scope graph formalism for name binding analysis. It constructs directed graphs where scopes are nodes connected by labeled edges, enabling IDE features like go-to-definition, find-references, and rename.

### Data Model

```haskell
data ScopeGraph = ScopeGraph
    { sgScopes :: Map ScopeId Scope
    , sgRoot   :: ScopeId
    , sgNextId :: Int
    , sgFile   :: Maybe FilePath
    }

data Scope = Scope
    { scopeId            :: ScopeId
    , scopeDeclarations  :: [Declaration]   -- names introduced here
    , scopeReferences    :: [Reference]     -- names referenced here
    , scopeEdges         :: [Edge]          -- connections to other scopes
    , scopeKind          :: ScopeKind
    }

data ScopeKind = FileScope | LetScope | AttrSetScope | RecAttrSetScope
               | FunctionScope | WithScope

data EdgeLabel = Parent | Import | With | Inherit | AttrAccess
```

### Construction

`fromNixExpr` walks the Nix AST and creates scope nodes for each scope-introducing form:

| Nix Form | Scope Kind | Behavior |
|---|---|---|
| Top-level file | `FileScope` | Root scope |
| `let ... in ...` | `LetScope` | Child scope; all bindings declared here |
| `{ a = ...; b = ...; }` | `AttrSetScope` | Child scope; attribute names are declarations |
| `rec { ... }` | `RecAttrSetScope` | Same as attrset but distinguished for tooling |
| `x: expr` / `{a, b} @ self: expr` | `FunctionScope` | Parameters declared here |
| `with expr; body` | `WithScope` | Linked to the expr scope via `With` edge |

Each child scope gets a `Parent` edge pointing back to its containing scope. References (`NSym`) are recorded in the current scope. Attribute accesses (`e.attr`) produce `AttrRef` records.

### Multi-File Graphs

`fromModuleGraph :: Map FilePath NExprLoc -> ScopeGraph` merges individual file graphs into a single global graph:
1. Build per-file scope graphs via `fromNixFile`
2. Merge via `mergeGraphs` — offset all scope IDs to avoid collisions
3. Add `Import` edges connecting file root scopes under a synthetic global root

### Resolution

```haskell
resolve :: ScopeGraph -> Reference -> Either ResolutionError Declaration
resolveAll :: ScopeGraph -> Either [ResolutionError] [(Reference, Declaration)]
```

Name resolution walks edges from the reference's scope:
1. Check the current scope for a matching declaration
2. If not found, traverse edges grouped by label in priority order: `Parent` → `Import` → `With` → `Inherit` → `AttrAccess`
3. First edge group that yields results wins (scoped search)
4. Multiple results in the same scope → `Ambiguous` error
5. No results anywhere → `Unresolved` error

**Cycle detection:** A `visited` set prevents infinite loops from cycles in the scope graph (e.g., recursive let bindings).

### Serialization

- **JSON:** Full `ToJSON` instances for all types via Aeson
- **Dhall:** `toDhall :: ScopeGraph -> Text` — exports via an intermediate export type to handle `Natural`-based IDs

### Queries

```haskell
declarationsInScope :: ScopeGraph -> ScopeId -> [Declaration]    -- all reachable declarations
referencesInScope :: ScopeGraph -> ScopeId -> [Reference]        -- refs in a specific scope
findDeclaration :: ScopeGraph -> Text -> [Declaration]           -- by name, global
findReferences :: ScopeGraph -> Declaration -> [Reference]       -- all refs to a declaration
```

---

## LSP Server

**Modules:** `Narsil.LSP.Server`, `Narsil.LSP.Handlers`

The LSP server exposes 15 handlers implementing the Language Server Protocol:

### Server Setup — `Narsil.LSP.Server`

```haskell
run :: IO Int
```

Minimal `runServer` configuration using the `lsp` library. Passes `staticHandlers` to the `Handlers` monoid from `Narsil.LSP.Handlers`.

### Handler Registry

```
handlers :: Handlers (LspM ())
handlers = mconcat
    [ notificationHandler SMethod_Initialized          initializedHandler
    , notificationHandler SMethod_TextDocumentDidOpen  documentOpenHandler
    , notificationHandler SMethod_TextDocumentDidChange documentChangeHandler
    , notificationHandler SMethod_TextDocumentDidSave  documentSaveHandler
    , notificationHandler SMethod_TextDocumentDidClose documentCloseHandler
    , requestHandler     SMethod_TextDocumentHover     hoverHandler
    , requestHandler     SMethod_TextDocumentDefinition definitionHandler
    , requestHandler     SMethod_TextDocumentRename    renameHandler
    , requestHandler     SMethod_TextDocumentReferences referencesHandler
    , requestHandler     SMethod_TextDocumentCompletion completionHandler
    , requestHandler     SMethod_TextDocumentSignatureHelp signatureHelpHandler
    , requestHandler     SMethod_TextDocumentCodeAction codeActionHandler
    , requestHandler     SMethod_TextDocumentDocumentSymbol documentSymbolHandler
    , requestHandler     SMethod_TextDocumentSemanticTokensFull semanticTokensFullHandler
    , requestHandler     SMethod_TextDocumentInlayHint inlayHintHandler
    ]
```

### 4-Pass Diagnostics Engine

The `fullLint` function runs four diagnostic passes in sequence:

| Pass | Lint Target | Module |
|---|---|---|
| `nixVios'` | General Nix lint (`rec`, `with`, raw mkDerivation, etc.) | `Narsil.Lint.Nix` |
| `derivVios'` | Derivation quality (`meta`, `description`) | `Narsil.Lint.Derivation` |
| `patternVios'` | Anti-patterns (`or null`, translateAttrs) | `Narsil.Lint.Patterns` |
| `embeddedBashDiags` | Forbidden bash constructs in embedded scripts | `Narsil.Lint.Forbidden` |

Diagnostics are published on every document open, change, and save event. On save and open, `voidProjectDiags` fires an async task (`async`) to run project-wide diagnostics via `buildModuleGraph`, but results are currently not published (WIP).

### Handler Features

**Hover** (`hoverHandler`):
- Type-inference at cursor via `inferExprAtWithEnv`
- Option documentation integration via `inferOptionAtPath` from `Narsil.Layout.ModuleSystem`
- Cross-module type env built via `buildCrossEnv` (walks project root → flake.nix → module graph)

**Definition** (`definitionHandler`):
- Builds cross-file scope graph via `buildCrossScopeGraphWith`
- Finds reference at cursor, resolves to declaration across files
- Returns `Location` with target file URI

**Rename** (`renameHandler`):
- Single-file scope graph for rename refactoring
- Resolves declaration, finds all references, produces `WorkspaceEdit` with all `TextEdit`s

**References** (`referencesHandler`):
- Cross-file scope graph resolution
- Returns declaration location + all reference locations across files

**Completion** (`completionHandler`):
- Builtin name completions from `builtinEnv` (with type signatures as detail)
- Module system option completions (with types)
- Scope-local variable completions (placeholder — currently returns [])
- Completion items include kind (`Function`, `Module`, `Property`) and detail text

**Signature Help** (`signatureHelpHandler`):
- Finds enclosing function call at cursor via `findEnclosingCall`
- Looks up builtin signature from `builtinEnv`
- Extracts parameter labels from curried `TFun` chains (up to 5 params)
- Returns `SignatureHelp` with active parameter tracking

**Code Actions** (`codeActionHandler`):
- Filters diagnostics overlapping the requested range
- Maps specific violation codes to suggested quick-fix actions:
  - ALEPH-N001 (`with`): "Replace `with` by explicit bindings"
  - ALEPH-N013 (missing meta): "Insert `meta` attribute"
  - ALEPH-N014 (missing description): "Add description to meta"
  - ALEPH-N009 (or null): "Replace `or null` by if-then-else"
  - ALEPH-N011 (writeShellScript): "Use writeShellApplication instead"
- Actions are all informational (no edits yet — marked preferred for auto-fix UX)

**Document Symbols** (`documentSymbolHandler`):
- Extracts top-level bindings from the AST
- Maps expression types to symbol kinds (Function, Object, Array, String, Number, Boolean, Variable)
- Includes child symbols for nested attribute sets

**Semantic Tokens** (`semanticTokensFullHandler`):
- Full tokenization of Nix source with 8 token types: keyword, function, variable, parameter, type, string, number, property
- 3 modifier types: definition, readonly, defaultLibrary
- Builtins are highlighted as functions with `defaultLibrary` modifier
- Reserved words (`if`, `then`, `else`, `let`, `in`, `with`, `rec`, `inherit`, `assert`, `import`) as keywords
- Delta-encoded output for efficient wire transfer

**Inlay Hints** (`inlayHintHandler`):
- Runs `inferExprWithEnv` with cross-module environment
- Produces `InlayHint` at each binding with `: <type>` annotation
- Only visible within the requested range
- Wraps `inferExprWithEnv` — if inference fails, produces empty hints (no error reported)

### Cross-Module Infrastructure

Two helpers build project-wide data structures on demand:

- `buildCrossEnv :: Uri -> IO TypeEnv` — finds project root (walking up to `flake.nix` or `.narsil.dhall`), builds module graph, populates `envImportTypes` with all module types, extends with both raw and resolved import paths.
- `buildCrossScopeGraphWith :: Uri -> Maybe NExprLoc -> IO ScopeGraph` — same project root discovery, builds module graph, constructs `fromModuleGraph` scope graph, optionally inserts the current buffer's expression into the map.

---

## Formatter (vendored nixfmt)

**Module:** `Narsil.Syntax.Format`

The `fmt` command reformats Nix source. The formatter is meaning-preserving and produces byte-for-byte the same output as the upstream `nixfmt` binary.

### Design

There is no hand-rolled pretty-printer. `Narsil.Syntax.Format` delegates to a **deep-vendored copy of nixfmt 1.3.1** (RFC 166), whose source lives under `vendor/nixfmt/` (MPL-2.0; see `vendor/nixfmt/LICENSE`) and is built as a private cabal sub-library, `nixfmt-vendored`. nixfmt re-parses the source with its own parser — its layout depends on comment/trivia attached to tokens, which hnix discards — so the already-parsed `NExprLoc` is *unused* by the formatter; the caller still parses with hnix first only to run the safety/depth gate.

Both entry points call:

```haskell
Nixfmt.format (layout 100 2 False) path srcTxt
```

i.e. the RFC-166 CLI defaults matching `nixfmt -`: **100-column width, 2-space indent, non-strict**. On the (unreachable) event that nixfmt's parser rejects source the caller already parsed with hnix, the formatter falls back to the input verbatim.

### Export API

```haskell
formatNix     :: Text -> NExprLoc -> Text              -- format from source text
formatNixFile :: Text -> FilePath -> NExprLoc -> Text  -- format file (path used for errors)
```

### Parity

Byte-exact parity with the `nixfmt` binary is enforced by the `tools/fmtparity/check.sh` harness (curated 14/14, real fixtures 48/48). The vendored modules (`Nixfmt`, `Nixfmt.Lexer`, `Nixfmt.Parser`, `Nixfmt.Predoc`, `Nixfmt.Pretty`, `Nixfmt.Types`, `Nixfmt.Util`, …) are first-class code in our tree: they build under our `-Wall -Werror` flags, and we own them — divergence happens by editing `vendor/nixfmt/Nixfmt/Pretty.hs` etc. directly (those edits remain MPL-2.0).

> The `prettyprinter` / `prettyprinter-ansi-terminal` libraries are still dependencies, but only for `Narsil.Syntax.Pretty` (terminal-colored CLI output) — not for the formatter.

---

## Type-Annotated Output

**Module:** `Narsil.Inference.Nix`

The `infer` command runs the inference engine over a Nix file and injects inferred types back into the source as `# :: <type>` comments. (`Narsil.Inference.Nix` is the *command renderer*; the inference *engine* it calls is `Narsil.Inference.Nix`.)

### Pipeline

```
Source text → hnix parse → inferExprWithEnv → annotateSource → annotated text
```

### Export API

```haskell
annotateFile        :: FilePath -> IO (Either Text Text)             -- default (no-import) env
annotateFileWithEnv :: TypeEnv -> FilePath -> IO (Either Text Text)  -- pre-built cross-module env
annotateExpr        :: Text -> Either Text Text                       -- annotate an in-memory expr
annotateSource      :: Text -> InferResult -> Text                    -- low-level injector
```

`annotateFile` (used by the `fmt`/`infer` CLI path via `cmdInfer`) defaults to `builtinEnv`; `annotateFileWithEnv` accepts a pre-built `TypeEnv` so the command does not throw away cross-module knowledge.

**`annotateSource`:**

1. Takes the original source text and the inference result (`irBindings :: [Binding]`)
2. Creates an `Ann` for each binding: a `# :: <type>` comment carrying `prettyType bindType`
3. Sorts annotations by location (reverse line order) so that insertions don't invalidate subsequent positions
4. Inserts each annotation on the line before the declaration, preserving the original indentation

Type values are rendered by `prettyType` / `prettyScheme` from `Narsil.Inference.Nix.Type` (union types as `a | b | c`, function types right-associative `a -> b`, string literals quoted).

---

## Layout Conventions

**Modules:** `Narsil.Layout.Convention`, `Narsil.Layout.Convention`, `Narsil.Layout.ModuleKind`, `Narsil.Layout.Naming`

### Convention Definition — `Narsil.Layout.Convention`

The layout convention system enforces structural constraints on project organization. A `Convention` defines:

```haskell
data Convention = Convention
    { convName           :: Text
    , convDescription    :: Text
    , convRules          :: [ConventionRule]
    , convFileNaming     :: NamingConvention   -- file naming (kebab-case, etc.)
    , convAttrNaming     :: NamingConvention   -- attribute naming
    , convIdentNaming    :: NamingConvention   -- identifier naming
    , convRequireFlakeMod :: Bool              -- require everything to be a flake module
    }
```

Each `ConventionRule` maps a `ModuleKind` to an expected location:

```haskell
data ConventionRule = ConventionRule
    { ruleKind      :: ModuleKind
    , rulePattern   :: PathPattern        -- where is it allowed?
    , ruleForbidden :: [PathPattern]      -- where is it banned?
    , ruleExportName :: Maybe Text        -- required export path
    }
```

**Path patterns:** `Prefix ["nix", "modules", "flake"]` matches paths starting with those components. `Exact`, `Contains`, and `AnyOf` provide additional matching modes.

**Built-in conventions:**

| Convention | Description |
|---|---|
| `straylight` | The straylight/aleph convention: flat module layout under `nix/` with kebab-case everywhere. Flake modules in `nix/modules/flake/`, NixOS modules in `nix/modules/nixos/`, packages in `nix/packages/`, etc. |
| `nixpkgsByName` | Nixpkgs `pkgs/by-name/` layout with camelCase attribute naming |
| `flakeParts` | Standard flake-parts layout (`modules/`, `packages/`, `overlays/`) |
| `nixosConfig` | NixOS system configuration layout (`modules/` or `hosts/`, `users/` or `home/`) |

**Naming conventions:**

```haskell
data NamingConvention = KebabCase | SnakeCase | CamelCase | PascalCase | NoNaming
```

`isValidName` checks a string against the convention. `toKebabCase` / `toSnakeCase` provide automatic conversion.

**Error codes:**
- `E001` — file in wrong location
- `E002` — file in forbidden location
- `E003` — wrong file name convention
- `E004` — wrong attribute name convention
- `E005` — wrong identifier convention
- `E006` — not a flake module (when required)
- `E007` — missing required export

### Layout Violation Checking — `Narsil.Layout.Convention`

Operates independently of the convention system for historical compatibility:

```haskell
findLayoutViolations :: FilePath -> NExprLoc -> [LayoutViolation]
findLayoutViolationsInDir :: FilePath -> IO [LayoutViolation]
```

Checks:
- **L001:** `_index.nix` files banned (module graph derived from directory structure)
- **L002:** `_main.nix` files banned (use explicit imports in `flake.nix`)
- **L003:** Module missing `_class` attribute
- **L004:** Wrong `_class` value for directory context
- `expectedModuleClass` derives expected class from path (e.g., `nix/modules/nixos/` → `"nixos"`)

### Integration with Module Graph

The module graph builder (`Narsil.Layout.Graph`) calls `findLayoutViolations` on each parsed file during `buildModules`, accumulating results in `LayoutFailure` and making them available through `hasViolations` and `totalViolationCount`.

---

## Effect Algebra

**Module:** `Narsil.Syntax.Effect`

Models Nix overlays using a coeffect-effect calculus, tracking what each overlay requires and produces.

```haskell
data Coeffect
    = RequireUpstream Text NixType     -- requires a definition from upstream
    | RequireSelf Text NixType         -- requires a definition from self layer
    | RequireImport FilePath           -- requires an imported file

data Effect
    = Define Text NixType     -- introduces a new definition
    | Override Text NixType   -- overrides an existing definition
    | Modify Text             -- modifies an existing definition in place

data OverlaySignature = OverlaySignature
    { osCoeffects :: Set Coeffect
    , osEffects   :: Set Effect
    }
```

**Operations:**

```haskell
mergeSignatures :: OverlaySignature -> OverlaySignature -> OverlaySignature
```
Merges two overlay signatures: new effects are unioned; new coeffects are added only if not already satisfied by existing effects.

```haskell
checkCompatibility :: Map Text NixType -> OverlaySignature -> [Text]
```
Checks that an overlay's upstream requirements are all present in a given base environment. Returns a list of missing dependency names.

The algebra is complete and tested; integration with actual Nix overlay analysis (walking `final: prev: { ... }` expressions and extracting signatures) is in progress.

---

## Configuration System

**Module:** `Narsil.Core.Config`

Configuration is loaded from Dhall files (default: `.narsil.dhall`) using the `dhall` library's auto-derivation via generics.

### Configuration Schema

```dhall
-- .narsil.dhall
{ profile = "standard"
, overrides =
  [ { id = "with-lib", severity = SevOff }            -- turn off with-lib rule
  , { id = "no-heredoc-in-inline-bash", severity = SevWarning }  -- downgrade
  ]
, extra-ignores = [ "*.md.nix", "test/**" ]             -- glob patterns
}
```

### Haskell Representation

```haskell
data Config = Config
    { configProfile      :: Text           -- profile name (currently advisory)
    , configExtraIgnores :: [Text]         -- glob ignore patterns
    , configOverrides    :: [RuleOverride] -- severity overrides for lint rules
    }

data RuleOverride = RuleOverride
    { overrideId       :: Text            -- rule identifier
    , overrideSeverity :: Severity        -- desired severity
    , overrideReason   :: Maybe Text      -- human-readable reason
    }

data Severity = SevOff | SevInfo | SevWarning | SevError
```

### Loading

```haskell
loadConfig :: FilePath -> IO (Either Text Config)
```

Reads a `.dhall` file, deserializes via `FromDhall` generics. Returns `defaultConfig` on any failure (profile `"standard"`, empty overrides, empty ignores).

The CLI `--config <path>` flag specifies an alternative config path. If absent, `.narsil.dhall` in the current directory is used. If neither exists, `defaultConfig` is used.

### Rule Suppression Pipeline

| Function | Purpose |
|---|---|
| `effectiveSeverity :: Config -> Text -> Maybe Severity` | Look up override for a rule |
| `isSuppressed :: Config -> Text -> Bool` | True if effective severity is `SevOff` |
| `isIgnored :: Config -> FilePath -> Bool` | True if file matches an ignore glob |
| `configIgnores :: Config -> [Text]` | Raw ignore patterns |

### Rule ID Mapping

Each lint module defines violation types that are mapped to text rule IDs:

| Domain | Module | Rule IDs |
|---|---|---|
| Bash forbidden | `Narsil.Lint.Forbidden` | `no-heredoc-in-inline-bash`, `no-eval`, `no-backtick` |
| Nix lint | `Narsil.Lint.Nix` | `with-lib`, `rec-anywhere`, `no-substitute-all`, `no-raw-mkderivation`, etc. |
| Derivation lint | `Narsil.Lint.Derivation` | `missing-meta`, `missing-description` |
| Package lint | `Narsil.Lint.Packages` | `default-nix-in-packages` |
| Pattern lint | `Narsil.Lint.Patterns` | `or-null-fallback`, `no-translate-attrs-outside-prelude` |
| Type checking | (built-in) | `type-check-failure` |

The CLI `check` command and LSP diagnostics both consult these mappings to determine which violations are suppressed.

### Glob Matching

A custom glob engine supports `*` and `**` patterns, normalized against file paths. Tokens are split by directory component, allowing `**` to match across directory boundaries.

---

## CLI Layer

### Entry Point — `app/Main.hs`

```
main :: IO ()
main = runLog InfoS $ do
    args <- liftIO getArgs
    let (maybeConfigPath, commandAndArgs) = parseConfigArg args
    config <- loadConfiguration maybeConfigPath
    dispatchCommand config commandAndArgs
```

**Argument parsing:**
- `--config <path>` is extracted first; everything after is the command
- If no `--config` is given, `loadConfiguration` tries `.narsil.dhall`, then falls back to `defaultConfig`
- `dispatchCommand` pattern-matches on `[command, ...]`

### Command Dispatch — `Narsil.CLI.Dispatch`

| Command | Handler | Description |
|---|---|---|
| `narsil check <path>` | `cmdCheck` | Auto-detect: directories run CI mode, `.nix` files run Nix check, others run bash check |
| `narsil fmt <file.nix>` | `cmdFmt` | Reformat via `Narsil.Syntax.Format` (delegates to vendored nixfmt) |
| `narsil infer <file.nix>` | `cmdInfer` | Type-infer then inject `# :: <type>` annotations via `Narsil.Inference.Nix` |
| `narsil emit <script.sh>` | `cmdEmit` | Parse bash, build schema, emit `emit_config()` function via `Narsil.Emit.Config` |
| `narsil lsp` | `cmdLSP` | Start LSP server via `Narsil.LSP.Server.run` |
| `narsil scope <file.nix>` | `cmdScope` | Build scope graph, pretty-print to terminal |
| `narsil scope --json <file.nix>` | `cmdScopeJSON` | Scope graph as JSON |
| `narsil scope --dhall <file.nix>` | `cmdScopeDhall` | Scope graph as Dhall |
| `narsil --help` / `-h` | `usage` | Print usage help |

### CI Mode — `Narsil.CLI.CI`

The `check` command on a directory triggers CI mode, which runs four phases:

```
runCIPhases config dir:
  1. runTypeCheckPhase  → type-check every .nix file in parallel
  2. runGraphPhase      → build module graph from flake.nix, collect lint + layout violations
  3. runNixPhase        → extract bash scripts from flake.nix, run bash pipeline on each
  4. runPackagePhase    → check package directory structure (default.nix presence)
```

**Phase 1 (Type Check)** uses `forConcurrently` with a `QSemN`-based concurrency limiter (capped at `getNumCapabilities`). Each file runs through `checkFile` with Katip context propagation.

**Phase 2 (Graph)** runs `buildModuleGraphFromFlake` and aggregates `mgLintFailures`.

**Phase 3 (Nix Bash)** re-uses the bash script extraction + checking infrastructure from `Narsil.CLI.Bash`.

**Phase 4 (Packages)** runs `LintPackages.checkPackageDirs` on all collected files.

The `reportCISummary` output shows files scanned, type-check pass/fail/skip counts, lint violations, package violations, bash violations, and graph failures.

### Single-File Type Check — `Narsil.CLI.Check`

```haskell
checkFile :: Config.Config -> FilePath -> AppM TCResult
```

For each file:
1. Parse via `Nix.parseNixFile`
2. Detect unsupported constructs (`rec attrset`, dynamic attribute access, dynamic `hasAttr`) — skip type check if found
3. Run `combinedLint` (Nix + Derivation + Pattern lint)
4. Partition violations through the config suppression system
5. Run `inferExpr`; handle results according to `type-check-failure` rule severity
6. Report diagnostics with file markers

**Violation partitioning** uses the config system: each violation is checked against `isSuppressed` using the appropriate rule ID mapping. Suppressed violations are discarded; active ones are reported.

### Bash File Checking — `Narsil.CLI.Bash`

```haskell
checkBashFile :: Config.Config -> FilePath -> AppM ()
checkNixFile :: Config.Config -> FilePath -> AppM ()
checkScript  :: Config.Config -> FilePath -> BashScript -> AppM Int
```

`checkBashFile` runs the full bash pipeline (parse → lint → facts → constraints → solve → report) on a standalone `.sh` file.

`checkNixFile` combines Nix type checking with embedded bash script analysis — extracts bash scripts via `NixParse.extractBashScripts`, then runs `checkScript` on each one.

`checkScript` handles a single extracted bash script: parses with ShellCheck, finds forbidden violations, checks interpolations for non-store-path references, extracts facts, generates constraints, solves, and reports bare commands, dynamic commands, and type errors.

### Reporting — `Narsil.CLI.Report`

```haskell
reportNixLintViolations :: FilePath -> [NixViolation] -> AppM ()
reportDerivViolations :: FilePath -> [DerivViolation] -> AppM ()
reportPatternViolations :: FilePath -> [PatternViolation] -> AppM ()
```

Each reporter formats violations with source locations, error codes (ALEPH-*), and contextual messages. Bare commands produce `ALEPH-B005`, dynamic commands produce `ALEPH-B006`.

### Logging — `Narsil.Core.Log`

```haskell
type AppM = KatipContextT IO
runLog :: Severity -> AppM a -> IO a
```

All CLI commands run in `AppM` (Katip context monad) with structured logging. `runLog` configures a stderr scribe with ANSI color support at the specified minimum severity. Template Haskell `$(logTM)` is used throughout for zero-boilerplate log statements with automatic source location.

---

## Search & Documentation — `Narsil.Docs.Extract`

Three modules provide documentation extraction and search:

- **`Narsil.Docs.Types`** — core types: `DocEntry` (name, summary, examples, source module), `SearchIndex`
- **`Narsil.Docs.Extract`** — extracts documentation from Nix modules (parse, find function bindings, extract comments/docstrings)
- **`Narsil.Docs.Search`** — full-text search over extracted documentation

---

## Module Dependency Map

The following diagrams show the import relationships between modules, organized by subsystem.

### Top-Level API

```
Narsil
├── Narsil.Bash.Types              (re-exported)
├── Narsil.Core.Config             (Config type + loadConfig + rule queries)
├── Narsil.Bash.Parse         (parseBash)
├── Narsil.Bash.Facts         (extractFacts)
├── Narsil.Inference.Nix.Constraint   (factsToConstraints)
├── Narsil.Inference.Nix.Unify        (solve)
└── Narsil.Inference.Bash.Schema       (buildSchema, validateConfigPaths)
```

### Bash Subsystem

```
Narsil.Bash.Parse ────────► ShellCheck (external)
Narsil.Bash.Patterns ─────► Narsil.Bash.Facts
Narsil.Bash.Facts ────────► Narsil.Bash.Patterns
                              ├─ Narsil.Bash.Parse
                              └─ Narsil.Bash.Types (Fact, Literal, Span)

Narsil.Bash.Builtins ─────► Narsil.Bash.Facts

Narsil.Inference.Nix.Constraint ──► Narsil.Bash.Types (Constraint, Fact)
Narsil.Inference.Nix.Unify ───────► Narsil.Bash.Types (Subst, Type, TypeVar)
Narsil.Inference.Bash.Schema ──────► Narsil.Bash.Types (Schema, Fact, Subst)
Narsil.Emit.Config ───────► Narsil.Bash.Types (Schema)
Narsil.Lint.Forbidden ────► Narsil.Bash.Parse
```

### Nix Subsystem

```
Narsil.Inference.Nix.Type ────────── (NixType, RowTail, Scheme, Subst, type helpers)
Narsil.Inference.Nix        (the HM inference engine)
├── Narsil.Inference.Nix.Type
├── Narsil.Syntax.Annotation
├── hnix (Expr.Types, Expr.Types.Annotated, Parser, Utils)
└── Narsil.Bash.Types (Span, Loc)

Narsil.Inference.Nix            (the `infer` command renderer)
├── Narsil.Inference.Nix (Binding, InferResult, inferExprWithEnv, builtinEnv)
├── Narsil.Syntax.Parse
├── Narsil.Inference.Nix.Type (prettyType)
└── Narsil.Core.Safety

Narsil.Syntax.Parse
├── Narsil.Syntax.Annotation
├── Narsil.Bash.Types
└── hnix (Parser, Expr.Types, Utils)

Narsil.Lint.Nix ─────────── Narsil.Syntax.Annotation
Narsil.Lint.Derivation ── Narsil.Syntax.Annotation
Narsil.Lint.Patterns ── Narsil.Syntax.Annotation + .Nix.Types
Narsil.Lint.Packages ── Narsil.Syntax.Annotation
Narsil.Lint.Combined ─── .Nix.Lint + .Nix.LintDerivation + .Nix.LintPatterns

Narsil.Layout.Graph
├── Narsil.Inference.Nix (inferExpr, inferExprWithEnv, builtinEnv, extendImport)
├── Narsil.Lint.Nix (findNixViolations)
├── Narsil.Layout.Convention (findLayoutViolations)
├── Narsil.Inference.Nix.Type
├── Narsil.Bash.Types (Loc, Span)
└── hnix (Parser, Expr.Types)

Narsil.Layout.ModuleSystem
├── Narsil.Inference.Nix.Type
└── hnix

Narsil.Layout.Import ────────── Narsil.Layout.Graph
Narsil.Layout.Scope
├── hnix (Expr.Types, Expr.Types.Annotated, Utils)
├── aeson (ToJSON)
└── dhall (ToDhall, inject)

Narsil.Layout.Convention ───────── hnix + Narsil.Bash.Types
Narsil.Layout.Convention ─ Narsil.Layout.ModuleKind + .Nix.Naming
Narsil.Layout.ModuleKind ───── (standalone, used by LayoutConvention + Module)
Narsil.Layout.Naming ───────── (standalone, used by LayoutConvention)
Narsil.Syntax.Effect ───────── Narsil.Inference.Nix.Type
Narsil.Syntax.Format ────── nixfmt-vendored (Nixfmt, Nixfmt.Predoc) + hnix (NExprLoc)
Narsil.Inference.Nix ────────── Narsil.Inference.Nix + .Nix.Parse + .Nix.Types + .Safety
Narsil.Syntax.Annotation ───────── Narsil.Bash.Types + hnix
```

### LSP Subsystem

```
Narsil.LSP.Server
├── Language.LSP.Server (lsp)
└── Narsil.LSP.Handlers

Narsil.LSP.Handlers
├── Language.LSP.Protocol.Types
├── Language.LSP.Server
├── Narsil.Bash.Parse (parseBash)
├── Narsil.Lint.Forbidden
├── Narsil.Inference.Nix (inferExprWithEnv, builtinEnv, extendImport, TypeEnv)
├── Narsil.Lint.Nix (findNixViolations)
├── Narsil.Lint.Derivation
├── Narsil.Lint.Patterns
├── Narsil.Layout.Graph (buildModuleGraph, mgModuleTypes)
├── Narsil.Layout.ModuleSystem (extractOptions)
├── Narsil.Syntax.Parse (findShellScriptCalls, extractString)
├── Narsil.Layout.Scope (fromModuleGraph, fromNixExpr, resolve, resolveAll, findReferences)
├── Narsil.Inference.Nix.Type
└── Narsil.Syntax.Annotation
```

### CLI Subsystem

```
app/Main ────────────────────── Narsil.CLI.Dispatch + .Config + .Log

Narsil.CLI.Dispatch
├── Narsil (parseScriptFile, scriptSchema)
├── Narsil.CLI.Bash (checkBashFile, checkNixFile)
├── Narsil.CLI.CI (cmdCI)
├── Narsil.Emit.Config (emitConfigFunction)
├── Narsil.LSP.Server (run)
├── Narsil.Inference.Nix (annotateFile)
├── Narsil.Syntax.Format (formatNixFile)
├── Narsil.Syntax.Parse (parseNixFile)
└── Narsil.Layout.Scope (fromNixFile, toDhall, toJSON)

Narsil.CLI.Bash
├── Narsil.Bash.Facts (extractFacts)
├── Narsil.Bash.Parse (parseBash)
├── Narsil.CLI.Check
├── Narsil.CLI.Report
├── Narsil.Inference.Nix.Constraint (factsToConstraints)
├── Narsil.Inference.Nix.Unify (solve)
├── Narsil.Lint.Forbidden (findViolations)
├── Narsil.Syntax.Parse (extractBashScripts)
└── Narsil.Inference.Bash.Schema (validateConfigPaths)

Narsil.CLI.Check
├── Narsil.CLI.Report (report*)
├── Narsil.Inference.Nix (inferExpr)
├── Narsil.Lint.Combined (combinedLint)
├── Narsil.Syntax.Parse (parseNixFile)
└── Narsil.Inference.Nix.Type (prettyType)

Narsil.CLI.CI
├── Narsil.CLI.Bash (parseNixFiles, checkScript)
├── Narsil.CLI.Check (checkFile)
├── Narsil.CLI.Report (report*)
├── Narsil.Lint.Nix (formatNixViolations)
├── Narsil.Lint.Packages (checkPackageDirs)
└── Narsil.Layout.Graph (buildModuleGraphFromFlake)
```

### Cross-Cutting Dependencies

```
Narsil.Bash.Types ◄─── Narsil (entire project)
Narsil.Core.Config ─── Narsil.Lint.Forbidden + .Nix.Lint + .Nix.LintDerivation
                         + .Nix.LintPackages + .Nix.LintPatterns
Narsil.Core.Log ────── CLI layer (all CLI modules)
```

**Key dependency rules:**
- `Narsil.Bash.Types` is the foundation — no other internal dependencies
- `Narsil.Inference.Nix.Type` depends only on `base`, `containers`, `text`, `hnix`
- `Narsil.Inference.Nix` (the HM engine) depends on `Narsil.Inference.Nix.Type` and `Narsil.Bash.Types`; `Narsil.Inference.Nix` (the `infer` command) sits on top of it
- `Narsil.Core.Config` depends on six lint modules (for rule ID mappings)
- The LSP module (`Narsil.LSP.Handlers`) is the most connected leaf, importing from 14+ internal modules

---

## Build and Test Infrastructure

### Build

- **Build system:** Cabal (`cabal-version: 3.0`, `narsil.cabal`)
- **GHC options:** `-Wall -Werror -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints` (GHC 9.10.3)
- **Language:** GHC2021
- **Source layout:** `lib/` (main library, 46 modules), `app/` (executable), `vendor/nixfmt/` (the private `nixfmt-vendored` sub-library)
- **Vendored sub-library:** `library nixfmt-vendored` (visibility `private`) builds the deep-vendored nixfmt 1.3.1 under `vendor/nixfmt/` (MPL-2.0) and is depended on by the main library. Its only deps are `base`, `containers`, `megaparsec`, `mtl`, `parser-combinators`, `scientific`, `text`, `transformers`, `pretty-simple`.

### Dependencies

| Category | Libraries |
|---|---|
| Nix parsing | `hnix >= 0.17 && < 0.18` |
| Bash parsing | `ShellCheck >= 0.9 && < 0.12` |
| Config | `dhall >= 1.42 && < 1.43` |
| LSP | `lsp >= 2.7 && < 2.8`, `lsp-types >= 2.3 && < 2.4` |
| Logging | `katip >= 0.8 && < 0.9` |
| Formatting | `nixfmt-vendored` (private sub-library; deep-vendored nixfmt 1.3.1) |
| Terminal pretty-printing | `prettyprinter >= 1.7 && < 1.8`, `prettyprinter-ansi-terminal >= 1.1 && < 1.2` (used only by `Narsil.Syntax.Pretty`) |
| Serialization | `aeson >= 2.0 && < 2.3` |
| Parsing (CLI) | `megaparsec >= 9.0 && < 10.0` |
| Data structures | `containers >= 0.6 && < 0.8`, `data-fix >= 0.3 && < 0.4` |
| Concurrency | `async >= 2.2 && < 2.3` |
| MTL | `mtl >= 2.3 && < 2.4`, `transformers >= 0.5 && < 0.7` |

### Test Suites

| Suite | Type | Description |
|---|---|---|
| `narsil-test` | QuickCheck property tests | Adversarial property testing of type inference, unify, constraint generation |
| `narsil-fixtures` | Fixture-based | Syntax and analysis test vectors |
| `narsil-more-fixtures` | Fixture-based | Additional analysis test vectors |
| `narsil-flake-parts` | Integration | Tests flake-parts project structures and module graph construction |
