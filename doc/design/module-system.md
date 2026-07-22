# The module system: from `TAny` to a real ontology

## The soft spot

Module-shaped files (`{ config, lib, pkgs, ... }: …`) currently get their
external parameters typed `TAny` (`moduleParamVar` / `envModuleParams`). That
was the right first move — those values arrive from the module system, not the
file — but it makes the checker *blind inside the single largest dialect of
Nix*. Every `config.services.foo.port` is dynamic, every option definition is
unchecked, and each precision round quietly builds on that sand. This document
is the load-bearing design.

## What the module system is (the semantics we model)

`lib.evalModules { modules = […]; }`:

1. **Collection** — modules are gathered transitively through `imports`
   (already discovered as `EFlakeImport` edges by the closure).
2. **Declaration merge** — each module's `options` subtree *declares* options:
   `options.a.b = mkOption { type = <types.*>; … }`. Declarations from all
   modules merge into one options tree.
3. **Definition fixpoint** — each module's `config` subtree (or its whole body
   minus reserved keys, in shorthand form) *defines* values. The `config`
   every module receives is the FIXPOINT: all definitions merged per-option,
   guided by the option's *type*, after `mkIf` guards and `mkOverride`
   priorities.

The crucial observation: **the module system carries a reified type language.**
`mkOption { type = types.listOf types.str; }` states the type at the value
level. We do not have to infer module types — we can *read* them, then hold
definitions to them. The option type in a declaration is the module system's
own contract; surfacing it statically is checking the program against itself.

## The `types.*` → `NixType` mapping

Syntactic, like `builtinsFieldScheme` — matched through `types.`, `lib.types.`,
and bare names (via `with types;` / `with lib.types;`):

| module type | NixType |
| --- | --- |
| `bool` | `TBool` |
| `int`, `ints.*`, `port` | `TInt` |
| `float` | `TFloat` |
| `number` | `TInt \| TFloat` |
| `str`, `string`, `lines`, `commas`, `separatedString _`, `strMatching _`, `passwdEntry _` | `TString` |
| `path`, `pathInStore` | `TPath \| TString` (the module system coerces) |
| `package`, `shellPackage` | `TDerivation` |
| `nullOr t` | `TNull \| t` |
| `listOf t` | `TList t` |
| `attrsOf t`, `lazyAttrsOf t` | open record, anon row (map-like: keys unknowable) |
| `enum [ l… ]` | union of literals (strings stay `TStrLit`) |
| `either a b`, `oneOf [ t… ]` | union |
| `submodule { options = … }` | the record of its option tree, recursively |
| `functionTo t` | `TAny -> t` |
| `uniq t`, `unique _ t`, `coercedTo _ _ t` | `t` |
| `anything`, `unspecified`, `attrs`, `raw`, `deferredModule` | `TAny` |
| unrecognized | `TAny` (never guess) |

`mkEnableOption _` declares `TBool`. `mkPackageOption …` declares
`TDerivation`.

## What we check (the consumer contract)

For a module file whose body declares options tree **T**:

1. **`config` gets a spine** — the parameter binds to an open record carrying
   T's paths at their declared types, anon-open everywhere else. `cfg =
   config.services.foo` then gives `cfg.port : Int` — real hover, real
   propagation. Undeclared paths stay dynamic (the rest of the option universe
   legitimately lives outside the file), so nothing new can false-positive.
2. **Definitions meet declarations** — the same file's `config` section (or
   shorthand body) is walked for definitions at declared paths; each value
   expression is inferred and unified with the declared type.
   `port = mkOption { type = int; }` + `config.…dName.port = "8080"` is a REAL
   error, caught statically. `mkIf`/`mkDefault`/`mkForce`/`mkMerge` are
   already type-transparent combinators in the lib table, so guarded and
   prioritized definitions check through unchanged.
3. **Files with no declarations keep today's behavior** — `config : TAny`.
   The upgrade is strictly additive; nothing ricochets.

## What we deliberately do NOT model (yet)

* **Cross-module fixpoint** — the received `config` reflects *all* modules;
  we type only the paths this closure declares and leave the rest open. The
  closure's `EFlakeImport` edges make closure-wide declaration merging a
  mechanical Phase 2 (union of the reachable files' trees).
* **The options ORACLE seam** — the endgame for select-typo detection is an
  evaluated `options.json` index (exactly the `envPkgsOracle` pattern: NixOS
  publishes its full option tree; a `envOptionsOracle` would make
  `config.services.nginx.enabel` a real missing-attribute error). The design
  reserves the seam; nothing here conflicts with it.
* **Priorities/ordering semantics** — `mkOverride` arithmetic and `mkOrder`
  affect *which* value wins, never its type; type-transparency is the correct
  abstraction level.

## Ricochet audit

The known test-suite touchpoints and why they hold:

* `prop_module_*` family — module-mode files *without* declarations are
  unchanged (`TAny`).
* M-tree `module-shaped-import` — no declarations involved; unchanged.
* The mutation ledger — no entry involves `options`; new MustCatch entries
  are added for the definition/declaration contract.
* The sweep — nixos/modules is the biggest population of declaring files;
  any wrong row in the `types.*` table shows up as a named FP class within
  one sweep. That is the apparatus doing its job — the reason to land this
  NOW rather than after more tail-polishing on top of `TAny`.
