# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                                     // hacking
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

#     "He'd operated on an almost permanent adrenaline high, a byproduct of
#      youth and proficiency, jacked into a custom cyberspace deck that
#      projected his disembodied consciousness into the consensual
#      hallucination that was the matrix."
#
#                                                                  — Neuromancer

Developer documentation for `narsil`. How the pieces fit together.


# ══════════════════════════════════════════════════════════════════════════════
#                                                                  // data flow
# ══════════════════════════════════════════════════════════════════════════════

The core pipeline for bash analysis:

```
                    ┌─────────────────────────────────────────┐
                    │              bash source                │
                    └─────────────────┬───────────────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────────┐
                    │       Bash.Parse (shellcheck AST)       │
                    └─────────────────┬───────────────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────────┐
                    │            Bash.Facts                   │
                    │  ─────────────────────────────────────  │
                    │  DefaultIs "PORT" (LitInt 8080)         │
                    │  Required "HOST"                        │
                    │  ConfigAssign ["server","port"] "PORT"  │
                    │  CmdArg "curl" "--timeout" "TIMEOUT"    │
                    │  BareCommand "curl"                     │
                    └─────────────────┬───────────────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────────┐
                    │          Infer.Constraint               │
                    │  ─────────────────────────────────────  │
                    │  TVar "PORT" :~: TInt                   │
                    │  TVar "TIMEOUT" :~: TInt                │
                    └─────────────────┬───────────────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────────┐
                    │            Infer.Unify                  │
                    │  ─────────────────────────────────────  │
                    │  Hindley-Milner unification             │
                    │  subst: {"PORT" -> TInt, ...}           │
                    └─────────────────┬───────────────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────────┐
                    │           Schema.Build                  │
                    │  ─────────────────────────────────────  │
                    │  facts + subst → Schema                 │
                    │  { env, config, commands, ... }         │
                    └─────────────────────────────────────────┘
```


# ══════════════════════════════════════════════════════════════════════════════
#                                                                // fact system
# ══════════════════════════════════════════════════════════════════════════════

Facts are the intermediate representation between parsing and type inference.
They are observations about the script, not types themselves.


## // fact // types

```haskell
data Fact
  = DefaultIs !Text !Literal !Span        -- ${VAR:-default}
  | DefaultFrom !Text !Text !Span         -- ${VAR:-$OTHER}
  | Required !Text !Span                  -- ${VAR:?}
  | AssignLit !Text !Literal !Span        -- VAR=literal
  | AssignFrom !Text !Text !Span          -- VAR=$OTHER
  | ConfigAssign !ConfigPath !Text !Quoted !Span  -- config.x.y=$VAR
  | ConfigLit !ConfigPath !Literal !Span  -- config.x.y=literal
  | CmdArg !Text !Text !Text !Span        -- curl --flag $VAR
  | UsesStorePath !StorePath !Span        -- /nix/store/...
  | BareCommand !Text !Span               -- curl (not a store path)
  | DynamicCommand !Text !Span            -- "$CMD" (computed command)
```


## // fact // extraction // rules

From `Bash.Facts`:

| Pattern | Fact Generated |
|---------|----------------|
| `${VAR:-default}` | `DefaultIs "VAR" (parse default)` |
| `${VAR:-$OTHER}` | `DefaultFrom "VAR" "OTHER"` |
| `${VAR:?msg}` | `Required "VAR"` |
| `VAR=literal` | `AssignLit "VAR" (parse literal)` |
| `VAR=$OTHER` | `AssignFrom "VAR" "OTHER"` |
| `config.x.y=$VAR` | `ConfigAssign ["x","y"] "VAR" Unquoted` |
| `config.x.y="$VAR"` | `ConfigAssign ["x","y"] "VAR" Quoted` |
| `curl --flag $VAR` | `CmdArg "curl" "--flag" "VAR"` |
| `/nix/store/.../curl` | `UsesStorePath (StorePath "...")` |
| `curl ...` | `BareCommand "curl"` |


# ══════════════════════════════════════════════════════════════════════════════
#                                                         // builtins database
# ══════════════════════════════════════════════════════════════════════════════

`Bash.Builtins` maintains a database of command argument types. This enables
type inference from command usage:

```bash
curl --connect-timeout "$TIMEOUT"    # TIMEOUT :: TInt
ssh -p "$PORT" -i "$KEY" host        # PORT :: TInt, KEY :: TPath
find . -maxdepth "$DEPTH"            # DEPTH :: TInt
```


## // adding // new // commands

Edit `lib/Narsil/Bash/Builtins.hs`:

```haskell
builtins :: Map Text CommandSchema
builtins = Map.fromList
  [ ("curl", CommandSchema
      { cmdDescription = "HTTP client"
      , cmdPositional = [ArgSpec TString False "URL"]
      , cmdArgs = Map.fromList
          [ ("--connect-timeout", ArgSpec TInt False "Connection timeout")
          , ("--retry", ArgSpec TInt False "Number of retries")
          , ("-o", ArgSpec TPath False "Output file")
          , ("--output", ArgSpec TPath False "Output file")
          ]
      })
  , ...
  ]
```

The `ArgSpec` type:

```haskell
data ArgSpec = ArgSpec
  { argType :: !Type        -- expected type
  , argRequired :: !Bool    -- is this required?
  , argDescription :: !Text -- documentation
  }
```


# ══════════════════════════════════════════════════════════════════════════════
#                                                    // cross-language inference
# ══════════════════════════════════════════════════════════════════════════════


## // interpolation // placeholders

When extracting bash from Nix `writeShellScript` calls, Nix interpolations are
replaced with placeholders:

| Interpolation Type | Placeholder Format |
|-------------------|-------------------|
| Store path (`${pkgs.curl}`) | `/nix/store/__nix_interp_N__` |
| Other (`${config.timeout}`) | `${__nix_interp_N__}` |

The key insight: non-store-path placeholders use bash variable syntax
`${__nix_interp_N__}` so that existing fact extraction picks them up.


## // type // flow

```
Nix source:
  writeShellScript "x" ''curl --timeout "${config.timeout}"''

After extraction (bash content):
  curl --timeout "${__nix_interp_0__}"

Fact extraction:
  CmdArg "curl" "--timeout" "__nix_interp_0__" span

Constraint generation:
  TVar "__nix_interp_0__" :~: TInt

Schema.env:
  "__nix_interp_0__" :: TInt

Mapping back to Nix:
  interps[0].intExpr = "config.timeout"
  → ${config.timeout} :: TInt
```


## // implementation // files

| File | Responsibility |
|------|----------------|
| `Nix.Parse` | `extractPartsWithInterps` — generates placeholders |
| `Nix.Parse` | `Interpolation` type with `intIndex` field |
| `Schema.Build` | `extractInterpTypes` — finds typed placeholders |
| `app/Main.hs` (dispatch in `CLI/Dispatch.hs`) | `mapInterpTypesToNix` — maps back to Nix exprs (n.b. symbol not found in current source; verify/rename) |


# ══════════════════════════════════════════════════════════════════════════════
#                                                              // nix inference
# ══════════════════════════════════════════════════════════════════════════════

Nix type inference in `Nix.Infer` implements Hindley-Milner with row polymorphism.


## // key // functions

```haskell
-- entry point: infer types for a file
inferFile :: FilePath -> IO (Either Text TypedExpr)

-- infer type for an expression in a context
infer :: Ctx -> NExprLoc -> InferM (Type, Subst)

-- unify two types
unify :: Type -> Type -> InferM Subst

-- generalize a type (let polymorphism)
generalize :: Ctx -> Type -> Scheme

-- instantiate a scheme with fresh variables
instantiate :: Scheme -> InferM Type
```


## // row // polymorphism

Open attribute sets (`TAttrsOpen`) allow extension:

```nix
# f :: { a : Int, ... } -> Int
f = x: x.a + 1

# valid: f { a = 1; b = 2; }
# the `b` field is allowed because the row is open
```

Closed attribute sets (`TAttrs`) are exact:

```nix
# g :: { a : Int } -> Int
g = x: x.a + 1

# invalid: g { a = 1; b = 2; }
# b is not allowed in a closed row
```


# ══════════════════════════════════════════════════════════════════════════════
#                                                               // scope graphs
# ══════════════════════════════════════════════════════════════════════════════

`Nix.Scope` builds scope graphs for tooling integration (IDE support, etc.).


## // scope // graph // structure

```haskell
data ScopeGraph = ScopeGraph
  { sgScopes :: Map ScopeId Scope
  , sgRoot :: ScopeId
  }

data Scope = Scope
  { scopeDefinitions :: [Definition]
  , scopeReferences :: [Reference]
  , scopeEdges :: [Edge]
  }

data Edge = Edge
  { edgeLabel :: EdgeLabel  -- Lexical | Import
  , edgeTarget :: ScopeId
  }
```


## // resolution

Name resolution follows edges in order:

1. Check local definitions in current scope
2. Follow `Lexical` edges to parent scopes
3. Follow `Import` edges to imported scopes


# ══════════════════════════════════════════════════════════════════════════════
#                                                                   // testing
# ══════════════════════════════════════════════════════════════════════════════


## // golden // tests

`test/Fixtures.hs` runs the tool on fixtures and compares output to expected:

```
test/fixtures/bash/check-by-name.sh
test/fixtures/bash/check-by-name.parse.expected
test/fixtures/bash/check-by-name.infer.expected
test/fixtures/bash/check-by-name.infer.json
```

To update expected outputs after intentional changes:

```bash
cabal run fixtures -- --bless
```


## // property // tests

`test/Props.hs` uses QuickCheck for algebraic properties:

```haskell
-- unification is reflexive
prop_unify_reflexive :: Type -> Property
prop_unify_reflexive t = unify t t === Right emptySubst

-- unification is symmetric
prop_unify_symmetric :: Type -> Type -> Property
prop_unify_symmetric t1 t2 =
  unify t1 t2 === unify t2 t1

-- substitution composition
prop_subst_compose :: Subst -> Subst -> Type -> Property
prop_subst_compose s1 s2 t =
  apply (compose s1 s2) t === apply s1 (apply s2 t)
```


## // adversarial // tests

`test/Adversarial.hs` tests security properties:

```haskell
-- injection attempts don't execute
prop_injection_safe :: InjectionAttempt -> Property
prop_injection_safe attempt =
  parseScript (inject attempt) `shouldNotCrash`

-- path traversal rejected
prop_traversal_blocked :: Property
prop_traversal_blocked =
  isStorePath "../../../etc/passwd" === False

-- integer overflow becomes string
prop_overflow_safe :: Property
prop_overflow_safe =
  parseLiteral "99999999999999999999" === LitString "..."
```


# ══════════════════════════════════════════════════════════════════════════════
#                                                         // adding // features
# ══════════════════════════════════════════════════════════════════════════════


## // new // bash // construct

1. Add pattern recognition in `Bash.Facts.extractFacts`
2. Add new `Fact` constructor if needed in `Types.hs`
3. Add constraint generation in `Infer.Constraint.factToConstraints`
4. Add schema building in `Schema.Build`
5. Add test fixture in `test/fixtures/bash/`
6. Run `cabal run fixtures -- --bless` to generate expected output


## // new // nix // construct

1. Add inference case in `Nix.Infer.infer`
2. Update scope graph construction in `Nix.Scope` if needed
3. Add formatter case in `Nix.Format` if type annotation needed
4. Add test fixture in `test/fixtures/nix/`


## // new // policy // rule

1. Add violation type in `Lint.Forbidden.ViolationType`
2. Add detection in `Lint.Forbidden.findViolations`
3. Document in `SPECIFICATION.md` section 5


# ══════════════════════════════════════════════════════════════════════════════
#                                                             // error handling
# ══════════════════════════════════════════════════════════════════════════════


## // error // codes

Error codes follow the pattern `NARSIL-BXXX`:

| Code | Category |
|------|----------|
| B001-B099 | Parsing errors |
| B100-B199 | Type errors |
| B200-B299 | Policy violations |
| B300-B399 | Lint warnings |

n.b. not all error codes are implemented yet — cf. `REVIEW.md`.


## // error // types

```haskell
data TypeError
  = UnificationError Type Type
  | OccursCheck TypeVar Type
  | UnboundVariable Text
  | ...

data LintError
  = Violation ViolationType Span
  | ...
```


# ══════════════════════════════════════════════════════════════════════════════
#                                                              // lean 4 port
# ══════════════════════════════════════════════════════════════════════════════

See `REVIEW.md` section "LEAN 4 PORT CONSIDERATIONS" for the full list.

Key items:

1. Replace `Text` with `String` (Lean's native)
2. Use `Except` monad instead of `Either`
3. Totality proofs for parsers
4. Separate pure and IO cleanly
5. Define proper `SourcePos` instead of conflating token IDs with lines


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

#     "Case had always taken it for granted that the real bosses, the
#      kingpins in a given industry, would be both more and less than
#      people... he'd seen it in the eyes of the men at the Sense/Net
#      boardroom: a rich emptiness, a palpable sense of unfinished things."
#
#                                                                  — Neuromancer

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                                 — b7r6 // 2026
