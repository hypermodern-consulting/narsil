# Configuration

narsil reads a Dhall configuration file from the project root: `.narsil.dhall`.

## Quick start

```
-- .narsil.dhall
{ profile = "standard"
, extra-ignores = [] : List Text
, overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >, reason : Optional Text }
}
```

Drop a copy at your project root and `narsil check .` picks it up automatically. Use `--config path/to/config.dhall` to specify a different file.

## Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `profile` | `Text` | Yes | One of `"strict"`, `"standard"`, `"minimal"`, `"nixpkgs"`, `"security"`, `"off"` |
| `extra-ignores` | `List Text` | Yes | Glob patterns for files/directories to skip |
| `overrides` | `List RuleOverride` | Yes | Per-rule severity overrides |

## Profiles

Profiles are built-in rule sets. Choose one as your starting point, then apply overrides to tune.

### strict

Full Aleph/Straylight conventions. Lisp-case enforcement, Dhall templating required, prelude-only wrappers. For new Straylight projects or projects that want maximum rigor.

```dhall
{ profile = "strict"
, extra-ignores = [ "vendor/**" ]
, overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >, reason : Optional Text }
}
```

Active: `with-lib` (Error), `rec-anywhere` (Error), `no-substitute-all` (Error), `no-raw-mkderivation` (Error), `no-raw-runcommand` (Error), `no-raw-writeshellapplication` (Error), `no-translate-attrs-outside-prelude` (Error), `no-heredoc-in-inline-bash` (Error), `missing-meta` (Error), `missing-description` (Error), `missing-class` (Error), `non-lisp-case` (Error), `cpp-using-namespace-header` (Error), `cpp-raw-new-delete` (Error).

### standard

Sensible defaults for most projects. No lisp-case, no prelude requirements. Catches real bugs and enforces best practices.

```dhall
{ profile = "standard"
, extra-ignores = [ "vendor/**", ".direnv/**" ]
, overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >, reason : Optional Text }
}
```

Active: `with-lib` (Error), `rec-anywhere` (Warning), `no-heredoc-in-inline-bash` (Error), `prefer-write-shell-application` (Warning), `missing-meta` (Warning), `missing-description` (Info), `missing-class` (Error), `cpp-using-namespace-header` (Error), `cpp-raw-new-delete` (Warning).

Silenced: `non-lisp-case`, `no-substitute-all`, `no-raw-mkderivation`, `no-raw-runcommand`, `no-raw-writeshellapplication`, `no-translate-attrs-outside-prelude`.

### minimal

Essential safety checks only. For legacy codebases or gradual adoption.

Active: `with-lib` (Warning), `no-heredoc-in-inline-bash` (Error), `missing-class` (Warning), `cpp-using-namespace-header` (Error).

Everything else: off.

### nixpkgs

For nixpkgs contributions. Extends `standard`, tuning for nixpkgs conventions: `with`/`rec` allowed, derivation quality enforced as Error, type inference downgraded to Warning.

### security

Security-focused. Extends `minimal`, emphasizes heredoc/eval/substitution checks as potential injection vectors, plus C++ memory safety rules at Error.

### off

All rules disabled, all files ignored. Use as a base for fully custom profiles.

```dhall
{ profile = "off"
, extra-ignores = []
, overrides = [ -- add your rules here ]
}
```

## Rule overrides

Each override targets a single rule by ID and sets its severity:

```
{ id = "rec-anywhere"
, severity = < Off | Info | Warning | Error >.Warning
, reason = Some "legacy codebase, plan to phase out"
}
```

The `reason` field is optional — use it to document why a rule is suppressed or downgraded. CI logs include it.

### Severity levels

| Level | Behavior |
|-------|----------|
| `Error` | Fails the build. Exit code 1. |
| `Warning` | Printed to stderr, does not fail the build. |
| `Info` | Printed to stdout, informational only. |
| `Off` | Suppressed entirely. |

## Rule ID reference

### Bash rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `no-heredoc-in-inline-bash` | ALEPH-B001, B002 | Error | Heredocs and here-strings in bash |
| `no-eval` | ALEPH-B003 | Error | `eval` in bash scripts |
| `no-backtick` | ALEPH-B004 | Error | Backtick command substitution |
| `no-bare-commands` | ALEPH-B005 | Error | Commands without store path prefix |
| `no-dynamic-commands` | ALEPH-B006 | Error | Computed/variable command names |

### Nix rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `with-lib` | ALEPH-N001 | Error | `with` expressions |
| `rec-anywhere` | ALEPH-N002 | Error | `rec` attrsets |
| `no-substitute-all` | ALEPH-N005 | Error | `substituteAll` calls |
| `no-raw-mkderivation` | ALEPH-N006 | Error | Direct `mkDerivation` |
| `no-raw-runcommand` | ALEPH-N007 | Error | Direct `runCommand` |
| `no-raw-writeshellapplication` | ALEPH-N008 | Error | Direct `writeShellApplication` |
| `prefer-write-shell-application` | ALEPH-N011 | Warning | `writeShellScript` over `writeShellApplication` |
| `long-inline-string` | ALEPH-N012 | Warning | Double-quoted strings > 120 chars |

### Derivation rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `missing-meta` | ALEPH-D001 | Warning | Derivations without `meta` attr |
| `missing-description` | ALEPH-D002 | Info | Derivations without `meta.description` |

### Pattern rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `or-null-fallback` | ALEPH-PT01 | Warning | `or null` fallback pattern |
| `no-translate-attrs-outside-prelude` | ALEPH-PT02 | Error | `mapAttrs`/`translateAttrs` outside prelude |

### Naming rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `non-lisp-case` | ALEPH-NM01 | Off (Error in strict) | Non-kebab-case identifiers |

### Module rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `missing-class` | ALEPH-L009 | Error | Missing `_class` attribute on module |

### Type rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `type-check-failure` | — | Error | Type inference errors |

### Package rules

| Rule ID | Error code | Default | What it catches |
|---------|-----------|---------|-----------------|
| `default-nix-in-packages` | ALEPH-P001 | Warning | Package dir without `default.nix` |

### C++ rules

| Rule ID | Default | What it catches |
|---------|---------|-----------------|
| `cpp-using-namespace-header` | Error | `using namespace` in headers |
| `cpp-raw-new-delete` | Warning | Raw `new`/`delete` calls |

## Ignores

Glob patterns that exclude files and directories from all checks:

```dhall
{ extra-ignores =
  [ "vendor/**"
  , "third-party/**"
  , ".direnv/**"
  , "test/fixtures/**"
  ]
, ...
}
```

Patterns use standard glob syntax: `*` matches within a single path segment, `**` matches across directories. The patterns are checked against paths relative to the project root.

## Layout convention

The layout convention is selected in the config — it determines directory structure enforcement:

```dhall
-- .narsil.dhall
{ profile = "standard"
, layout = < straylight | nixpkgs-by-name | flake-parts | nixos-config >.straylight
, extra-ignores = [] : List Text
, overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >, reason : Optional Text }
}
```

See [Layout Conventions](./layout-conventions.md) for the full specification of each convention and the module kind detection pipeline.

| Convention | Enforces |
|------------|----------|
| `straylight` | `nix/modules/{flake,nixos,home,darwin}/`, `nix/packages/`, kebab-case files, `_class` attrs |
| `nixpkgs-by-name` | `pkgs/by-name/<sh>/<name>/package.nix` for packages only |
| `flake-parts` | `modules/`, `nixos-modules/`, `packages/`, `overlays/` |
| `nixos-config` | `hosts/`, `modules/`, `users/`, `home/` for NixOS/Home modules |

## Complete examples

### New Straylight project

```dhall
{ profile = "strict"
, extra-ignores = [ "result/**" ]
, overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >, reason : Optional Text }
}
```

### Existing project, gradual adoption

```dhall
{ profile = "minimal"
, extra-ignores =
  [ "vendor/**"
  , "third-party/**"
  , "docs/**"
  ]
, overrides =
  [ { id = "with-lib"
    , severity = < Off | Info | Warning | Error >.Info
    , reason = Some "tracking in issue #1234, fixing over next quarter"
    }
  ]
}
```

### CI with strict security

```dhall
{ profile = "security"
, extra-ignores = []
, overrides =
  [ { id = "no-heredoc-in-inline-bash"
    , severity = < Off | Info | Warning | Error >.Error
    , reason = Some "block deployment on any injection risk"
    }
  ]
}
```
