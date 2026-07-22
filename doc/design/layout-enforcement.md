# Layout enforcement & the `all-flake-module` convention

Status: **implemented**. `all-flake-module` plus the four prior conventions each
have an 8-pass / 8-fail fixture tree, all green under `narsil-layout`.

## Goal

Make layout-convention enforcement a first-class, fixture-tested feature, and add
a new convention — **`all-flake-module`** — modeled on
[`github:nixified-ai/flake`](https://github.com/nixified-ai/flake): every `.nix`
under `flake-modules/` is a flake-parts module that wires its children via
`imports`, with leaf `package.nix` derivations.

## The existing machinery (recap)

`Narsil.Nix.LayoutConvention` already models conventions:

- **`Convention`** — `{ convName, convDescription, convRules, convFileNaming,
  convAttrNaming, convIdentNaming, convRequireFlakeMod }`.
- **`ConventionRule`** — `{ ruleKind :: ModuleKind, rulePattern :: PathPattern,
  ruleForbidden :: [PathPattern], ruleExportName :: Maybe Text }`. A file of a
  given `ModuleKind` must live where `rulePattern` says and not where
  `ruleForbidden` says.
- **`PathPattern`** — `Prefix | Contains | Exact | AnyOf | None`.
- **`ModuleKind`** — `NixOSModule | HomeModule | DarwinModule | Package | Overlay
  | FlakeModule | FlakePart | Library | Flake | DevShell | Test | Unknown`.
- **`detectKind path expr`** — classifies a file by its `_class` attr, then
  filename hints, then structural hints.
- **`validateFileFromExpr conv root path expr :: [LayoutError]`** — runs:
  `checkBannedFiles`, `validateLocation`, `validateForbidden`, `validateFileName`,
  `validateFlakeModReq`. Error codes `E001`–`E007`.
- Selected by name via `layoutFromName` (`Config.effectiveLayout . configLayout`).

Built-in conventions today: `straylight`, `nixpkgs-by-name`, `flake-parts`,
`nixos-config`. `convRequireFlakeMod` exists and is enforced by
`validateFlakeModReq` (E006) but no shipped convention sets it `True` yet.

## The `all-flake-module` convention

### The nixified-ai pattern (observed)

```
flake.nix                              -- flake-parts.lib.mkFlake { … } { imports = [ ./flake-modules ]; … }
flake-modules/default.nix              -- { imports = [ ./website ./fetchers ./models … ]; }
flake-modules/website/default.nix      -- { perSystem = { pkgs, … }: { packages.website = …; }; }
flake-modules/website/package.nix      -- { stdenv, … }: stdenv.mkDerivation { … }   (callPackage leaf)
flake-modules/fetchers/default.nix     -- { imports = [ ./fetchair ./fetchresource ]; }
flake-modules/fetchers/fetchair/default.nix
flake-modules/packages/<name>/package.nix
```

Defining properties:

1. The only top-level Nix file is `flake.nix`, which goes through
   `flake-parts.lib.mkFlake` and imports the `flake-modules/` tree.
2. Every directory under `flake-modules/` has a `default.nix` that is a
   **flake-parts module** — an attrset (or `{ … }:` function to one) containing
   `imports` / `perSystem` / `flake` / `_module` keys.
3. Modules wire their children with `imports = [ ./child … ];`.
4. Leaf derivations are `package.nix` files (`callPackage` targets), co-located
   with the module that builds them.

### Convention model

```haskell
allFlakeModule = Convention
  { convName = "all-flake-module"
  , convDescription = "Every .nix is a flake-parts module under flake-modules/ (nixified-ai)"
  , convRules =
      [ ConventionRule FlakeModule (Prefix ["flake-modules"]) [] Nothing
      , ConventionRule Package     (Prefix ["flake-modules"]) [] Nothing
      ]
  , convFileNaming = NoNaming        -- files are default.nix / package.nix
  , convAttrNaming = NoNaming
  , convIdentNaming = NoNaming
  , convRequireFlakeMod = True
  }
```

Enforced rules (what FAILS):

- **E006 — not a flake module.** Under `all-flake-module`, a recognized file that
  is not `Flake` / `FlakeModule` / `Package` (e.g. a stray `NixOSModule`,
  `Overlay`, bare attrset, or raw expression) is rejected. *(Requires relaxing
  `validateFlakeModReq` to also permit `Package` — a `package.nix` leaf is
  legitimate.)*
- **E00x — wrong location.** A `FlakeModule` or `Package` outside
  `flake-modules/` (e.g. `modules/foo.nix`, top-level `package.nix`) violates
  `rulePattern`.
- Banned files / constructs (`checkBannedFiles` + inherited lint) still apply.

### Required code change

`validateFlakeModReq` currently rejects any kind ∉ {Flake, FlakeModule, Unknown}.
Add `Package` to the allow-set so leaf `package.nix` files pass under a
`convRequireFlakeMod` convention. Register `"all-flake-module"` in
`layoutFromName`.

## Fixtures & test plan

Each convention gets **8 passing + 8 failing** fixtures. Modules are
near-empty — they exist to exercise *placement and shape*, not behavior:

```
test/fixtures/layout/<convention>/pass/<case>.nix      -- 0 layout errors
test/fixtures/layout/<convention>/fail/<case>.nix      -- ≥1 layout error
```

A test suite (`narsil-layout`) walks each convention's `pass`/`fail` tree,
runs `validateFileFromExpr (layoutFromName conv) root path expr` per file, and
asserts: every `pass/` file yields `[]`; every `fail/` file yields a non-empty
list. (Where a fixture's *relative path under the convention root* is what the
rules see — e.g. `pass/flake-modules/website/default.nix`.)

### `all-flake-module` — the 16

**pass (≥0 errors):** `flake.nix` (mkFlake + imports); `flake-modules/default.nix`
(imports children); `flake-modules/website/default.nix` (perSystem module);
`flake-modules/website/package.nix` (callPackage leaf); `flake-modules/models/default.nix`
(imports + perSystem); nested `flake-modules/fetchers/default.nix` →
`flake-modules/fetchers/fetchair/default.nix`; `flake-modules/packages/foo/package.nix`.

**fail (≥1 error):** a flake-module at the repo root (not under `flake-modules/`);
a `package.nix` at the repo root; a NixOS module under `flake-modules/` (not a
flake module → E006); a bare attrset (`{ a = 1; }`) under `flake-modules/`; a raw
expression (`1 + 1`) under `flake-modules/`; an overlay placed loose under
`flake-modules/`; a flake-logic file under `modules/` (wrong tree); a
`default.nix` that is a NixOS module rather than a flake module.

The same harness covers the other shipped conventions, each with its own 8 + 8
tree under `test/fixtures/layout/<convention>/`:

- **`straylight`** — kebab-case everywhere; `_class` required under
  `nix/modules/{flake,nixos,home,darwin}/`. Fail cases exercise E001 (wrong
  location), E002 (forbidden location), E003 (naming), E009 (missing `_class`),
  E010 (wrong `_class`).
- **`flake-parts`** — flake modules under `modules/` or `flake-modules/`,
  packages under `packages/`, overlays under `overlays/`; `_class` required under
  `modules/nixos/`. Fail cases cover wrong location, missing/wrong `_class`, and
  banned `_index.nix` / `_main.nix` (E007/E008).
- **`nixpkgs-by-name`** — the single Package rule (`pkgs/by-name/…`); fail cases
  are mislocated packages plus banned files.
- **`nixos-config`** — NixOS modules under `modules/` or `hosts/`, home modules
  under `users/` or `home/`; `_class` required under `modules/{nixos,home}/`.

All five run from one table in `test/Layout.hs`: 40 pass fixtures yield no errors,
40 fail fixtures each yield ≥1.

All five run from one table in `test/Layout.hs`: 40 pass fixtures yield no errors,
40 fail fixtures each yield ≥1.

## Self-enforcement (dogfooding)

The fixtures above test the layout *engine* (`validateFileFromExpr` called
directly). Separately, the layout convention is now enforced against real
project trees through the CLI:

- **`runLayoutPhase`** (in `CLI/CI.hs`) is a new phase of `narsil check
  <dir>`. It walks every on-disk `.nix` file via `collectFiles` (honoring
  `extra-ignores`) and validates each against `effectiveLayout` using the
  **project root** as the convention root — so a file's path relative to the
  root is what the location rules see, and stray/orphan files are caught.
  Violations render as unified clippy diagnostics (`error[E001] …`).
- This fixed two bugs that made the prior wiring inert: the module graph only
  reached files via `import ./x` applications (flake-parts wires modules as bare
  path literals, so nothing was discovered), and it passed `takeDirectory path`
  as the root (collapsing every relative path to its basename). Layout is no
  longer routed through the import graph at all.
- **This repo conforms to `flake-parts`** (`.narsil.dhall` sets `layout =
  "flake-parts"`). Its flake-module lives at `flake-modules/default.nix`
  (imported by `flake.nix`), which the convention accepts. `narsil check .`
  reports zero layout violations; moving that file, adding a stray module at the
  root, or dropping an `_index.nix` makes it fail.
- **`tools/layoutcheck/check.sh`** is an end-to-end guard: it runs the real
  binary against `test/fixtures/layout-projects/{good,perturbed}/` (two complete
  flake-parts projects) and asserts the clean one emits no layout diagnostics
  while the perturbed one emits the planted `E001`/`E007` violations and exits
  non-zero. This exercises CLI → `cmdCI` → `runLayoutPhase` end to end.

> Note: `narsil check .` still reports two `error[TYPE]` findings on the
> flake-parts `mkFlake` entrypoint — a pre-existing limitation of the type
> inferencer on flake-parts flakes, independent of layout. Layout enforcement is
> green regardless.

## Plan (done)

1. **This doc** (committed first).
2. **Fixtures** — `all-flake-module` pass/fail tree, then the other four.
3. **Green** — relax `validateFlakeModReq` for `Package`, register the convention,
   add the `narsil-layout` harness; `pass/` clean, `fail/` flagged.
4. **Self-enforcement** — `runLayoutPhase`, repo conformance to `flake-parts`,
   and the `tools/layoutcheck` end-to-end guard.
