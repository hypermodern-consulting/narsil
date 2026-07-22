# Nix Type Inference

narsil implements Hindley-Milner type inference with row polymorphism for the Nix expression language. The engine lives in `lib/Narsil/Nix/Inference.hs` (module `Narsil.Nix.Inference`) with support types in `lib/Narsil/Nix/Types.hs`.

## 1. Type system

### Grammar

```
NixType ::=
    TVar TypeVar              -- unification variable (α, β, ...)
  | TInt                      -- integer
  | TFloat                    -- float
  | TBool                     -- boolean
  | TString                   -- string (interpolated or unknown content)
  | TStrLit "text"            -- known string literal
  | TPath                     -- filesystem path
  | TNull                     -- null
  | TDerivation               -- derivation (build recipe)
  | TAny                      -- top type (escapes the type system)
  | TList NixType             -- homogeneous list
  | TFun NixType NixType      -- function (curried)
  | TRec (Map Text (NixType, Bool)) RowTail -- record with a row tail
  | TUnion [NixType]          -- sum / union (least upper bound)

RowTail ::=
    RClosed                   -- exactly the known fields
  | ROpen TypeVar             -- "at least these fields", tail is a row variable
```

Records are `TRec fields tail`. The actual `NixType` constructor is `TRec`, but the engine works through two bidirectional pattern synonyms defined in `Types.hs`:

- `TAttrs fields` ≡ `TRec fields RClosed` — a closed record (exact fields)
- `TAttrsOpen fields` ≡ `TRec fields (ROpen _)` — an open record (row polymorphism). As a *constructor*, `TAttrsOpen` uses the anonymous sentinel row var (`anonRowVar`); inference sites that need field accumulation build `TRec fields (ROpen r)` with a FRESH `r` via `mkOpenRec`.

Each field carries a `Bool` marking it optional (`?` in Nix patterns, e.g. `{ name, value ? 42 }:`). Non-optional fields trigger an error if missing or unmatched during unification.

### Type schemes

```
Scheme = Forall [TypeVar] NixType
```

Type schemes bind zero or more universally quantified type variables over a monotype. `Forall [] TInt` is a monomorphic scheme (no polymorphism). A scheme like `Forall [α, β] (α -> β -> α)` represents `∀α β. α → β → α`.

Schemes are instantiated at each use site by replacing quantified variables with fresh `TVar` values — this is the core of HM let-polymorphism.

### Substitution

```
type Subst = Map TypeVar NixType
```

A substitution maps type variables to types. Key operations (in `Types.hs`):

- `singleSubst v t` — singleton mapping `{v ↦ t}`
- `composeSubst s1 s2` — composes two substitutions: `s1 ∘ s2`, applying `s1` to values in `s2` then unioning (defined but **not** used by the engine; see below)
- `applySubst s t` — applies a substitution to a type, chasing transitive chains on read (the `TVar` case recurses through bound vars; it also resolves and merges row tails)
- `applySubstScheme s (Forall vars t)` — applies to a scheme, skipping the quantified variables

The inference state carries a global substitution (`inferSubst`). Crucially the engine keeps a **triangular** substitution: `addSubst v t` (in `Inference.hs`) is a plain `Map.insert`, **not** an eager `composeSubst`. The old composing form re-walked and rewrote the entire accumulated substitution on every bind (O(n) per bind, O(n²) over a program), so it was dropped (RC4). Resolution still fully normalises because `applySubst` chases transitively on read. Every caller binds `v` to a `t` that has already been resolved against the current substitution (via `applyCurrentSubst` in `unify` / `mergeTypes` / `unifyRec`) and runs the occurs check before insert, so the substitution stays acyclic and the on-read chase terminates.

## 2. Row polymorphism

### TAttrs (closed)

`TAttrs fields` represents a fully-determined record. All fields are known statically. This is the type of literal attribute sets:

```nix
{ x = 1; y = "hello"; }
-- type: TAttrs { x: Int, y: String }
```

Unifying two `TAttrs` requires the same keys. Any key present in one but not the other with `required=True` triggers a type error.

### TAttrsOpen (open)

`TAttrsOpen fields` represents a record with *at least* the given fields, possibly more. This models the "..." (variadic) pattern:

```nix
{ x, y, ... }: x + y
-- param type: TAttrsOpen { x: Int, y: Int }
-- full type:  TFun (TAttrsOpen { x: Int, y: Int }) Int
```

When a function parameter is declared as a set pattern with `Variadic`, the inference engine creates an open record via `mkOpenRec` (in `Inference.hs`), i.e. `TRec fields (ROpen r)` with a fresh row var `r` (see the `ParamSet` case of `inferLambda`). This way, if the function is called with `{ x = 1; y = 2; z = 3; }` the extra field `z` does not cause a type error — open records accept unknown fields.

### Unification rules for records

All record unification goes through `unifyRec` (in `Inference.hs`), which dispatches on the two row tails:

| lhs tail | rhs tail | rule |
|----------|----------|------|
| `RClosed` | `RClosed` | exact: delegated to `unifyAttrs` — unify shared field types, required fields must exist on both sides |
| `ROpen` | `RClosed` | `unifyCommon`, then `closeAgainst`: the open side's own required fields must exist in the closed side; the open tail var then absorbs the closed side's extra fields and is bound CLOSED |
| `RClosed` | `ROpen` | symmetric to above |
| `ROpen` | `ROpen` | `unifyCommon`, then both tail vars are bound to a SHARED fresh tail carrying each side's extra fields — so the field UNION is preserved across the unification |

The anonymous sentinel row var (`isAnonRowVar`) is never bound, so pure flake/module display types keep their open-world behavior (no field accumulation).

The bool flag (optional) propagates across unification: a closed side may legitimately omit a field that the open side marks optional.

### The `//` operator (NUpdate)

The attrset update operator `a // b` is handled in the `NUpdate` case of `inferBinary` (in `Inference.hs`). It merges two records (right overrides left):

```nix
{ x = 1; } // { y = 2; }
-- type: TAttrs { x: Int, y: Int }
```

If either operand is open, the result is open (built with a fresh row var via `mkOpenRec`). The right-hand fields override left-hand fields of the same name. When an operand is a bare type variable (e.g. `\x. x // { a = 1; }`), it is constrained to an open record rather than collapsed to the other operand's exact shape — so such functions stay polymorphic.

## 3. Let-polymorphism via SCC-based generalization

### The problem

Naive let-inference without generalization would monomorphize every binding. Consider:

```nix
let id = x: x;
in { a = id 1; b = id true; }
```

Without generalization, `id` would be inferred as `Int → Int` at its first use and the second use would fail. HM let-polymorphism solves this by generalizing the let-binding's type, producing the scheme `∀α. α → α`.

### SCC grouping

Mutually recursive bindings must be inferred together. `inferLet` (in `Inference.hs`) uses `Data.Graph.stronglyConnComp` to partition let-bindings into strongly connected components:

1. Each binding is parsed into `(name, expr, span)` triples via `parseBinding` (in `Inference.hs`)
2. `collectFreeVars` walks the expression to find its free variable references
3. `buildEdge` connects each binding name to the bound names it references
4. `stronglyConnComp` returns SCCs sorted in dependency order
5. Each SCC is inferred stepwise via `inferLetGroup`

### Generalization algorithm

After inferring a group of bindings, `generalize` (in `Inference.hs`) computes the type scheme:

1. Apply the current substitution to resolve all type variables
2. Collect free type variables in the binding's type: `freeTypeVars t'`
3. Collect free type variables in the *environment* (all previously-bound names): `freeInEnv`
4. Quantify over `freeInT \ freeInEnv` — only variables not mentioned by outer scopes
5. Package as `Forall quantifiedVars t'`

This follows the standard HM rule: generalise over variables not free in the environment. Acyclic SCCs (single bindings without self-reference) and cyclic SCCs (mutually recursive groups) are both generalised after inference, then added to the environment for use by downstream bindings and the body.

### Recursive bindings

Recursive (`rec { }`) and mutually-recursive `let` groups are handled identically: all names in the SCC are pre-allocated fresh type variables and inserted into scope, then each binding is inferred against that extended environment and unified with its pre-allocated variable. After unification, `inferLetGroup` (and `inferRecursiveBindings` for `rec` attrsets) runs an inline check that each variable resolved to something concrete — if *every* var in the group remains its original bare `TVar`, the group is self-referencing with no external constraint (e.g. `rec { x = x; }`), which is flagged as `"infinite type: rec bindings ... have no concrete constraint"`.

## 4. Unification algorithm

### Core unify (`unify` / `unify'` in `Inference.hs`)

`unify t1 t2` applies the current substitution to both types, then dispatches to `unify'` for structural comparison:

| case | action |
|------|--------|
| `TVar v` with `t` | `bindVar v t` (with occurs check) |
| `TAny` with `_` | vacuously true — TAny is the top type |
| `TInt`/`TFloat`/`TBool`/`TString`/`TPath`/`TNull`/`TDerivation` | succeed when identical |
| `TString` with `TStrLit _` | string literals are subtypes of string (both directions ok) |
| `TStrLit` with `TStrLit` | succeed (lit-to-lit is allowed) |
| `TList a` with `TList b` | unify `a` with `b` |
| `TFun a1 b1` with `TFun a2 b2` | unify `a1` with `a2`, then `b1` with `b2` |
| `TRec m1 tl1` with `TRec m2 tl2` | `unifyRec` — per the row polymorphism rules above |
| `TUnion ts` with `t` | `unifyUnion` — check that `t` is a member of the union (or a TVar) |
| `TFun` with attrs | attempt `__functor` protocol resolution (`unifyFunctor`) |
| anything else | type mismatch error |

### Occurs check (`occursCheck` in `Inference.hs`)

Prevents infinite types (e.g. `λx. x x`) by checking whether a type variable appears free inside the type it's being bound to. `occursCheck v t` traverses all compound types (`TList`, `TFun`, `TRec` — including its row tail var — and `TUnion`) and checks for `v`. It is invoked from `bindVar` (signalling `"infinite type"`) and from `bindRowVar` (signalling `"recursive row type"`).

### Type merging (join / LUB)

`mergeTypes` (in `Inference.hs`) differs from `unify` — instead of asserting equality, it computes a common supertype:

| case | result |
|------|--------|
| `TVar` with `t` | bind var → t, return t |
| `TAny` with `_` | return `TAny` |
| `TList a` with `TList b` | merge elements, produce `TList (merge a b)` |
| `TFun a1 b1` with `TFun a2 b2` | unify domains, merge codomains |
| `TAttrs m1` with `TAttrs m2` | field-by-field merge via `mergeAttrs` |
| identical types | return as-is |
| otherwise | produce `TUnion [a, b]` |

`mergeAttrs` (in `Inference.hs`) unions the keysets: shared keys get their types merged; keys present in only one record are marked optional. The result is always a closed `TAttrs`.

This is used for `if-then-else` branches (`inferIf`) and list elements (`inferList`) — anywhere two types must coexist rather than be proven equal. (The `//` operator does its own row-based merge in `inferBinary`, not via `mergeTypes`.)

## 5. Special handling

### Import resolution (`inferAppWithImport` in `Inference.hs`)

`inferAppWithImport` intercepts function applications where the argument is a path literal. Before falling through to standard application inference (`inferApp`), it checks `envImportTypes` — a cache of previously-imported modules' types (populated by `extendImport`). If the import path is known, the cached type is returned directly (after substitution). This enables cross-module type inference without re-parsing and re-inferring imported files.

Path extraction via `extractImportPathLiteral` handles literal paths (`NLiteralPath`), double-quoted string paths, and indented (`''…''`) string paths.

### With-scope resolution (`inferWith` / `inferSymbol` in `Inference.hs`)

`inferWith` evaluates the scope expression, stashes its type in `envWith` on the environment, then infers the body. The memo cache (`inferWithMemo`) is reset for the duration of each `with` block and restored afterward.

When `inferSymbol` encounters a name that isn't in the explicit environment, it falls back to `envWith` (its `resolveWithScope` helper):

1. Check the memo cache — if the field was already constrained, reuse the cached type
2. Otherwise: allocate a fresh `TVar`, constrain the (resolved) scope type to contain that field via `fieldConstraint`, apply substitution to resolve, and cache the result

(If neither the env nor a `with` scope provides the name, `inferSymbol` errors with `"unbound variable"` unless the environment is in lenient mode — `envLenient` — in which case it returns a fresh var.)

`fieldConstraint` (in `Inference.hs`) handles three cases:
- **Scope is `TAttrs`/`TAttrsOpen`**: look up the field; if found, unify the value type with the expected type (a miss is a no-op)
- **Scope is `TVar`**: unify the scope variable against an open record `TRec { name: valueT } (ROpen r)` — this constrains the scope type to be a record containing at least that field
- **Otherwise**: no-op (if scope type is, say, `TInt`, the `with` resolved to nothing meaningful)

The memo cache prevents repeated unification of the same field name, making `with` resolution practical for large scopes.

### Functor protocol (`unifyFunctor` in `Inference.hs`)

Nix supports callable attribute sets through the `__functor` convention: if an attrset contains a field `__functor` whose type is a function, that attrset is callable. `unifyFunctor` checks the attrset for `__functor :: TFun _ innerT` and unifies `innerT` with the expected function type.

This is triggered when `unify'` encounters a `TFun` ↔ attrset mismatch:

```nix
let mkSetter = {
  __functor = self: x: x + 1;
};
in mkSetter 5          -- resolves via functor protocol
```

If `__functor` is present but not a `TFun`, an explicit error is raised. If absent, the standard type mismatch error fires.

## 6. Builtin type signatures

The inference environment starts from `builtinEnv` (in `Inference.hs`), which provides type signatures for the modeled Nix builtins. Builtins come in two flavours:

- **Monomorphic** builtins (`toString`, `add`, arithmetic, predicates, I/O, etc.) live in the `builtinsTypes` table inside `builtinEnv`. They are stored as `Forall [] type` and don't vary across use sites.
- **Polymorphic** builtins are NOT monomorphic. The list builtins (`head`, `tail`, `map`, `filter`, `elemAt`, `length`, `concatLists`, `concatMap`, `foldl'`) and the row builtins (`attrNames`, `attrValues`, `hasAttr`, `getAttr`, `removeAttrs`) are held as **schemes** in the top-level `builtinSchemeTable` and instantiated fresh at each use site, so they are not prematurely monomorphized. (The `builtinsTypes` table also carries `TAny`-shaped fallbacks for these names; the schemes take priority via `builtinBindings`.)

Each name is bound both individually and under the top-level `"builtins"` key — an attrset (`TAttrs builtinsTypes`) for `builtins.<name>` selection. Because that record can only hold monotypes, `builtins.head`, `builtins.map`, `builtins.attrNames`, etc. are intercepted in the `NSelect` dispatch (`builtinsFieldScheme`) and instantiated fresh from `builtinSchemeTable` instead of read out of the record — keeping `builtins.attrNames` row-polymorphic.

### String / path conversions

| builtin | signature |
|---------|-----------|
| `toString` | `Int \| Float \| Bool \| Path \| String → String` |
| `baseNameOf` | `Path → String` |
| `dirOf` | `Path → Path` |
| `stringLength` | `String → Int` |
| `substring` | `Int → Int → String → String` |
| `replaceStrings` | `[String] → [String] → String → String` |

### List operations

| builtin | signature |
|---------|-----------|
| `head` | `[a] → a` |
| `tail` | `[a] → [a]` |
| `length` | `[a] → Int` |
| `elemAt` | `[a] → Int → a` |
| `filter` | `(a → Bool) → [a] → [a]` |
| `map` | `(a → b) → [a] → [b]` |
| `foldl'` | `(a → b → a) → a → [b] → a` |
| `concatLists` | `[[a]] → [a]` |
| `concatMap` | `(a → [b]) → [a] → [b]` |

### Attribute set introspectors

| builtin | signature |
|---------|-----------|
| `attrNames` | `∀ρ. { | ρ } → [String]` |
| `attrValues` | `∀ρ. { | ρ } → [Any]` |
| `hasAttr` | `∀ρ. String → { | ρ } → Bool` |
| `getAttr` | `∀ρ. String → { | ρ } → Any` (value-dependent, so result stays `Any`) |
| `removeAttrs` | `∀ρ. { | ρ } → [String] → { | ρ }` |
| `listToAttrs` | `[{ name: String, value: Any }] → Any` (monomorphic, in `builtinsTypes`) |

The row builtins above are schemes over a row variable `ρ` (their argument is an open record `{ | ρ }`), held in `builtinSchemeTable`. `listToAttrs` is monomorphic.

### Type predicates

| builtin | signature |
|---------|-----------|
| `isNull` | `a → Bool` |
| `isInt` | `a → Bool` |
| `isFloat` | `a → Bool` |
| `isBool` | `a → Bool` |
| `isString` | `a → Bool` |
| `isList` | `a → Bool` |
| `isAttrs` | `a → Bool` |
| `isFunction` | `a → Bool` |
| `isPath` | `a → Bool` |

### Arithmetic, I/O, and control flow

| builtin | signature |
|---------|-----------|
| `add`, `sub`, `mul`, `div` | `Int → Int → Int` |
| `lessThan` | `Int → Int → Bool` |
| `import` | `Path → a` |
| `readFile` | `Path → String` |
| `toPath` | `String → Path` |
| `derivation` | `{ ... } → Derivation` |
| `throw`, `abort` | `String → a` |
| `trace` | `String → a → a` |
| `seq`, `deepSeq` | `a → b → b` |
| `tryEval` | `a → { success: Bool, value: a }` |

### How signatures interact with inference

At lookup time `inferSymbol` calls `instantiate`, which allocates fresh `TVar`s for any quantified variables of a scheme. For monomorphic builtins (`Forall [] type`) this is a no-op. For the polymorphic schemes in `builtinSchemeTable`, instantiation produces fresh type/row variables at each use site — so `map : ∀α β. (α → β) → [α] → [β]` gets independent `α`, `β` everywhere it appears, and `attrNames : ∀ρ. { | ρ } → [String]` gets a fresh row var. This is what prevents premature monomorphization.

The `builtinsTypes` entries also carry a `Bool` flag; it is the field-optionality flag (always `False`/required here) shared with the record representation, not a "purity" marker.

## 7. Example inference walkthroughs

### Example 1: simple function

```nix
x: x + 1
```

1. **Lambda**: allocate fresh `α` for `x`, extend env with `x : α`
2. **Body** (`x + 1` via `NPlus`): infer `x` → `α`, infer `1` → `Int`. Unify `α = Int`. Return `Int`.
3. Result: `α → Int` with `α = Int` → `Int → Int`

### Example 2: set pattern with default

```nix
{ name, value ? 42 }: "${name}: ${toString value}"
```

1. **ParamSet**: `name` gets fresh `α`, `value` gets `Int` (from the default `42`)
2. Variadic → `TAttrsOpen { name: α, value: Int }`
3. **Body**: string interpolation → `TString`, context constrains `name : String` so `α = String`
4. Result: `TFun (TAttrsOpen { name: String, value: Int }) String`

### Example 3: let-polymorphism

```nix
let id = x: x; in { a = id 1; b = id true; }
```

1. **SCC**: single acyclic component `[id]`
2. **Infer `id`**: param gets fresh `α`, body returns `α` → type `α → α`
3. **Generalize**: `freeInT = {α}`, `freeInEnv = {}` → `Forall [α] (α → α)`
4. **Body `id 1`**: instantiate → `β → β`, unify `β = Int` → result `Int`
5. **Body `id true`**: instantiate → `γ → γ`, unify `γ = Bool` → result `Bool`
6. **Merge**: `mergeTypes Int Bool` → `TUnion [Int, Bool]`
7. Result: `TAttrs { a: Int, b: Bool }`

### Example 4: with scope

```nix
with lib;
let sum = foldl' add 0;
in sum [1 2 3]
```

1. **Infer `lib`**: suppose it has type `TAttrs { foldl': (a→b→a)→a→[b]→a, add: Int→Int→Int, ... }`
2. **`inferWith`**: store scope type in `envWith`, reset memo cache
3. **`foldl'` lookup**: not in explicit env, falls to `resolveWithScope` → fresh `α`, constrain `lib` field → resolved as `(a→b→a)→a→[b]→a`
4. **`add` lookup**: not in explicit env, memoized from step 3 → `Int→Int→Int`
5. Subsequent lookups in the same `with` block hit the memo cache, avoiding re-unification

### Example 5: functor protocol

```nix
let mkAdder = {
  __functor = self: x: y: x + y;
};
in mkAdder 2 3
```

1. **`mkAdder`**: `TAttrs { __functor: α → Int → Int → Int }`
2. **`mkAdder 2`**: `inferApp` expects `TFun arg result` for `mkAdder`, gets `TAttrs { ... }`
3. **`unify`** sees `TFun` / `TAttrs` mismatch → `unifyFunctor`
4. **`unifyFunctor`** finds `__functor : α → Int → Int → Int`, extracts inner `TFun Int (TFun Int Int)` → unifies app result with `Int → Int`
5. Result: `Int → Int` at `mkAdder 2`, `Int` at `mkAdder 2 3`

## 8. Inference environment

### TypeEnv structure

```
TypeEnv = TypeEnv
  { envBindings    :: Map Text Scheme      -- explicit name → scheme bindings
  , envWith        :: Maybe NixType        -- active `with` scope type
  , envImportTypes :: Map FilePath NixType -- cached imported module types
  , envLenient     :: Bool                 -- treat unbound names as fresh vars
  }
```

`envLenient` defaults to `False` (strict). When `True`, unbound names become fresh polymorphic vars instead of errors — used for compatibility with libraries that mention builtins we don't yet model.

### Name resolution priority (`inferSymbol`)

1. `lookupEnv` — explicit bindings (let, lambda params, builtins)
2. `envWith` — active `with` scope (with memo cache)
3. If `envLenient`, a fresh type variable; otherwise an `"unbound variable"` error

### Cross-module inference

```haskell
extendImport :: FilePath -> NixType -> TypeEnv -> TypeEnv
extendImport path t env = env{envImportTypes = Map.insert path t (envImportTypes env)}
```

When a file is imported, its inferred type is registered. Subsequent import applications (`import ./other.nix`) short-circuit to the cached type, avoiding redundant inference. This is handled transparently in `inferAppWithImport`.

## 9. Error reporting

Errors are reported via `ExceptT Text` in the `Infer` monad. `throwTypeError` (in `Inference.hs`) annotates the message with the current source span (line, column):

```haskell
throwTypeError :: Text -> Infer a
throwTypeError msg = do
    mSpan <- gets inferSpan
    case mSpan of
        Just (Span (Loc l c) _ _) ->
            throwError $ T.pack (show l) <> ":" <> T.pack (show c) <> ": " <> msg
        Nothing -> throwError msg
```

Error categories:

| error | trigger |
|-------|---------|
| `type mismatch: expected X, got Y` | unification of incompatible types (`typeMismatch`) |
| `infinite type: α occurs in β → α` | occurs check failure during `bindVar` (self-application) |
| `recursive row type: α occurs in ...` | occurs check failure during `bindRowVar` |
| `infinite type: rec bindings x, y have no concrete constraint` | a recursive/`rec` group that never resolves (e.g., `rec { x = x; }`) |
| `missing required field: name` | closed/closed: one side lacks a non-optional field required by the other |
| `unexpected field (required in other): name` | symmetric to above |
| `closed record missing field required by open record: name` | open/closed: the closed side lacks a non-optional field the open side requires |
| `__functor must be a function, got X` | attrset has `__functor` field but it's not a `TFun` |
| `type mismatch: expected one of A \| B, got C` | union membership check failed (`unifyUnion`) |
| `unbound variable: name` | name resolves in neither env nor `with` scope, and `envLenient` is off |
| `attribute 'k' missing on closed attribute set (...)` | selecting an absent key from a closed record (no `or` default) |
| `cannot select attribute 'k' from non-attrset type X` | selecting from a concrete non-record (e.g. `(x: x.foo) 5`) |
| `` operator `+` cannot combine X and Y `` | `+` on two concrete operands outside its lattice |

All spans originate from the Nix parser's source locations, propagated into `InferState` via `withSpan` (in `Inference.hs`) as expressions are recursively traversed.

## 10. Limitations and skipped constructs

The following Nix constructs are **not** type-inferred. The CLI's `detectUnsupportedConstruct` function (in `lib/Narsil/CLI/Check.hs`) identifies them before inference runs and skips the file to avoid producing incorrect results:

| construct | why skipped |
|-----------|-------------|
| `rec { }` attrsets (anywhere) | recursive attribute sets; the static field model can't represent their self-referential scope soundly |
| dynamic attribute access `x.${expr}` | the selected field name cannot be determined statically |
| dynamic attribute test `x ? ${expr}` | same — the tested field name is not statically known |

These are pragmatic decisions: attempting to infer types for these cases would require either a radically different approach (symbolic execution) or would produce unsound results.

Note that `with expr;` is **not** in this list — the inference engine handles `with` scopes directly (see §5), so files using `with` are still inferred.
