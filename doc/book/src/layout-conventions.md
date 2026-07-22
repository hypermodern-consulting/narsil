# Layout Conventions

Layout conventions enforce directory structure, file placement, and naming rules for Nix projects. Each convention defines a mapping from `ModuleKind` to expected file locations, forbidden zones, required flake exports, and naming style.

## Selection

Set the convention in your config:

```
-- .narsil.dhall
{ layout = <straylight | nixpkgs-by-name | flake-parts | nixos-config>
, ...
}
```

The convention is checked on `narsil check` and surfaced in-editor via LSP.

## Module kinds

Every `.nix` file is classified into a `ModuleKind` before layout rules are applied:

| Kind | Description |
|------|-------------|
| `FlakeModule` | flake-parts module (`perSystem`, `flake`) |
| `NixOSModule` | NixOS configuration module |
| `HomeModule` | home-manager module |
| `DarwinModule` | nix-darwin module |
| `Package` | Derivation / package definition |
| `Overlay` | Nixpkgs overlay (`final: prev:`) |
| `Library` | Library of helper functions |
| `Shell` | devShell or shell environment |
| `Flake` | `flake.nix` itself |
| `Test` | Test file |
| `Unknown` | Could not determine |

Each convention defines rules only for the kinds it cares about. Files with a kind that has no matching rule pass without validation.

---

## Built-in conventions

### `straylight`

Strict, comprehensive convention. `nix/` root directory, kebab-case for files, attributes, and identifiers. Every module kind has a dedicated subdirectory.

```
flake.nix
nix/
  modules/
    flake/       gpu-broker.nix       # FlakeModule
    nixos/       workstation.nix      # NixOSModule
    home/        tmux.nix             # HomeModule
    darwin/      defaults.nix         # DarwinModule
  packages/
    hello/
      default.nix                      # Package
  overlays/
    mesa.nix                           # Overlay
  lib/
    utils.nix                          # Library
  shells/
    default.nix                        # Shell
```

| Module kind | Location | Flake export | Forbidden |
|---|---|---|---|
| `FlakeModule` | `nix/modules/flake/` | — | `nix/packages/` |
| `NixOSModule` | `nix/modules/nixos/` | `flake.nixosModules` | `nix/packages/` |
| `HomeModule` | `nix/modules/home/` or `nix/modules/home-manager/` | `flake.homeModules` | `nix/packages/` |
| `DarwinModule` | `nix/modules/darwin/` | `flake.darwinModules` | — |
| `Package` | `nix/packages/<name>/default.nix` | `perSystem.packages` | `nix/modules/` |
| `Overlay` | `nix/overlays/` | `flake.overlays` | — |
| `Library` | `nix/lib/` | `flake.lib` | — |
| `Shell` | `nix/shells/` | `perSystem.devShells` | — |
| `Flake` | `flake.nix` (exact) | — | — |

Packages require a `default.nix` in their directory. Imported attribute names are expected to be kebab-case.

### `nixpkgs-by-name`

Nixpkgs `pkgs/by-name` layout. Has a rule for `Package` only — everything else is unconstrained.

```
flake.nix
pkgs/
  by-name/
    he/hello/package.nix       # Package
    ri/ripgrep/package.nix
```

| Module kind | Location | Flake export |
|---|---|---|
| `Package` | `pkgs/by-name/<sh>/<name>/package.nix` | — |

No naming enforcement for files. Attribute naming is camelCase (nixpkgs convention). All other module kinds (`NixOSModule`, `Library`, etc.) have no rules — they pass regardless of location.

### `flake-parts`

Flat structure following standard flake-parts conventions. No file naming enforcement.

```
flake.nix
modules/          gpu-broker.nix      # FlakeModule
flake-modules/    apps.nix            # FlakeModule (alt)
nixos-modules/    workstation.nix     # NixOSModule
modules/nixos/    server.nix          # NixOSModule (alt)
packages/
  hello/
    default.nix                        # Package
overlays/
  mesa.nix                             # Overlay
```

| Module kind | Location | Flake export |
|---|---|---|
| `FlakeModule` | `modules/` or `flake-modules/` | — |
| `NixOSModule` | `modules/nixos/` or `nixos-modules/` | `flake.nixosModules` |
| `Package` | `packages/<name>/` | `perSystem.packages` |
| `Overlay` | `overlays/` | `flake.overlays` |

`HomeModule`, `DarwinModule`, `Library`, `Shell`, `Flake`, and `Test` have no rules — unconstrained placement.

### `nixos-config`

Minimal convention for NixOS system configuration repositories. Only two module kinds are recognized:

```
flake.nix
hosts/            mythoform.nix       # NixOSModule
modules/          gpu-broker.nix      # NixOSModule
users/            b7r6/default.nix    # HomeModule
home/             default.nix         # HomeModule
```

| Module kind | Location | Flake export |
|---|---|---|
| `NixOSModule` | `modules/` or `hosts/` | — |
| `HomeModule` | `users/` or `home/` | — |

No naming enforcement. No forbidden locations. Any file not detected as `NixOSModule` or `HomeModule` is `Unknown` and passes without check.

---

## Module kind detection

Detection runs through three tiers, in priority order.

### Tier 1: `_class` attribute

A file can declare its kind explicitly with `_class = "<kind>"` in its top-level attrset:

```nix
{ config, lib, pkgs, ... }:
{
  _class = "nixos";
  options = { ... };
  config = { ... };
}
```

Valid `_class` values: `flake`, `nixos`, `home`, `homeManager`, `darwin`, `package`, `overlay`, `lib`, `shell`.

When `_class` is present, detection stops — the declared kind is used with 100% confidence. Everything else is ignored.

`homeManager` is an alias for `HomeModule`.

### Tier 2: Structural heuristics

If no `_class` attribute exists, the parser inspects the AST:

| Pattern | Detection confidence |
|---|---|
| `final: prev: { ... }` → two-argument function with overlay param names | 95% |
| Top-level `{ options = ...; config = ...; }` binding pair | 85% |
| Body calls `mkDerivation` or similar build function | 90% |
| Has `pname` + `version` attributes | 75% |
| Has `perSystem` or `flake` as a top-level binding | 80% |
| Function params include `config`, `lib`, `pkgs` (NixOS-module-like) | 70% |
| Function params include `stdenv`, `fetchurl`, etc. (package-like) | 70% |
| Function params include `self`, `inputs` (flake-module-like) | 60% |
| Has `buildInputs` + `shellHook` | 70% |
| Exports `mkOption`, `mkIf`, `mapAttrs` (library-like) | 50% |
| Has `options` but no `config` | 60% |

### Tier 3: Filename hints

Filnames act as weak signals — used when structural detection is inconclusive or as tiebreakers:

| Filename | Suggests | Confidence |
|---|---|---|
| `flake.nix` | `Flake` | 100% |
| `shell.nix` | `Shell` | 90% |
| `package.nix` | `Package` | 80% |
| `overlay.nix` | `Overlay` | 80% |
| `test.nix` | `Test` | 80% |
| `*-test.nix` | `Test` | 70% |
| `module.nix` | `NixOSModule` | 60% |
| `*-module.nix` | `NixOSModule` | 60% |
| `default.nix` | — (no signal) | — |

Evidence from structure and filename is combined. When rules conflict for the same kind, structural hints take priority. If no tier produces a match, the file is `Unknown`.

---

## Validation pipeline

For each `.nix` file, `validateFile` runs four checks in sequence:

| Step | Check | Error code |
|---|---|---|
| 1. Location | Is the file in a directory allowed for its detected kind? | `E001` |
| 2. Forbidden | Is the file in a directory banned for its kind? | `E002` |
| 3. File naming | Does the filename match the convention's naming style? | `E003` |
| 4. Flake module | If convention requires uniform structure, is this a flake module? | `E006` |

Additional validations run per-file on parsed content:

- **Attribute naming** (`E004`): Do exported attribute names match the convention's naming style?
- **Identifier naming** (`E005`): Do identifiers (variable names) match the convention's naming style?
- **Export presence** (`E007`): Is the file wired to the expected flake output?

Only files whose `ModuleKind` has a matching rule in the convention are checked. If no rule matches the detected kind, the file passes silently.

### Error codes

| Code | Message |
|---|---|
| `E001` | File in wrong location for module kind |
| `E002` | File in forbidden location |
| `E003` | File name violates naming convention |
| `E004` | Attribute name violates naming convention |
| `E005` | Identifier violates naming convention |
| `E006` | File must be a flake module |
| `E007` | Missing required export |

---

## Naming conventions

Each convention specifies three naming dimensions:

| Dimension | Controls | Examples |
|---|---|---|
| `convFileNaming` | `.nix` file basenames (minus `.nix` suffix) | `gpu-broker`, `snake_case` |
| `convAttrNaming` | Attribute names in exported sets | `nixosModules`, `homeModules` |
| `convIdentNaming` | Variable and binding identifiers in Nix code | `mkDerivation`, `buildInputs` |

Available styles:

| Style | Pattern | Example |
|---|---|---|
| `KebabCase` | lowercase + hyphens, no leading/trailing hyphens, no doubles | `gpu-broker`, `flake-parts` |
| `SnakeCase` | lowercase + underscores | `gpu_broker`, `flake_parts` |
| `CamelCase` | lowercase start, alphanumeric | `gpuBroker`, `flakeParts` |
| `PascalCase` | uppercase start, alphanumeric | `GpuBroker`, `FlakeParts` |
| `NoNaming` | no validation | — |

### Convention style matrix

| Convention | Files | Attributes | Identifiers |
|---|---|---|---|
| `straylight` | `KebabCase` | `KebabCase` | `KebabCase` |
| `nixpkgs-by-name` | `NoNaming` | `CamelCase` | `CamelCase` |
| `flake-parts` | `NoNaming` | `NoNaming` | `NoNaming` |
| `nixos-config` | `NoNaming` | `NoNaming` | `NoNaming` |

---

## Path patterns

Each `ConventionRule` uses path patterns to match directories. Patterns are matched against the file's relative path split into components.

| Pattern | Matches when |
|---|---|
| `Prefix ["nix", "modules", "flake"]` | Path starts with `nix/modules/flake/` |
| `Exact ["flake.nix"]` | Path is exactly `flake.nix` |
| `Contains ["modules"]` | Any component equals `modules` |
| `AnyOf [pat1, pat2, ...]` | Any sub-pattern matches |
| `None` | Always matches (no constraint) |

Forbidden patterns work the same way — if a forbidden pattern matches, the file is rejected with `E002`.

---

## Custom conventions

Define a `Convention` value in Haskell:

```haskell
import Narsil.Nix.LayoutConvention
import Narsil.Nix.ModuleKind

myConvention :: Convention
myConvention = Convention
    { convName = "my-org"
    , convDescription = "My org's layout"
    , convRules =
        [ ConventionRule
            { ruleKind = Package
            , rulePattern = Prefix ["pkgs"]
            , ruleForbidden = [Prefix ["modules"]]
            , ruleExportName = Just "perSystem.packages"
            }
        , ConventionRule
            { ruleKind = NixOSModule
            , rulePattern = AnyOf
                [ Prefix ["modules", "nixos"]
                , Prefix ["hosts"]
                ]
            , ruleForbidden = []
            , ruleExportName = Just "flake.nixosModules"
            }
        ]
    , convFileNaming = KebabCase
    , convAttrNaming = KebabCase
    , convIdentNaming = KebabCase
    , convRequireFlakeMod = False
    }
```

Each `ConventionRule` maps one `ModuleKind` to:

| Field | Purpose |
|---|---|
| `ruleKind` | Which `ModuleKind` this rule applies to |
| `rulePattern` | Where files of this kind must live |
| `ruleForbidden` | Directories this kind must never appear in |
| `ruleExportName` | Flake output where this module should be wired (or `Nothing`) |

Set `convRequireFlakeMod = True` to enforce that all files are flake-parts modules — any non-Flake and non-FlakeModule file will emit `E006`.
