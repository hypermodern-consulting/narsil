# Configuration

narsil reads a Dhall configuration file from the project root:
**`.narsil.dhall`** (the legacy name `.nix-compile.dhall` is still honored
everywhere it was consulted). Pass `--config <file.dhall>` to use a
different file. With no file at all, built-in defaults apply
(`profile = "standard"`).

```dhall
-- .narsil.dhall
{ profile = "standard"
, layout = "flake-parts"
, extra-ignores = [ "vendor/**" ]
, overrides =
    [ { id = "rec-anywhere"
      , severity = < Off | Info | Warning | Error >.Warning
      , reason = Some "legacy codebase, phasing out"
      }
    ]
, lsp = { max-threads = 4, max-memory-mb = 256, max-disk-mb = 512 }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `profile` | `Text` | One of `"strict"`, `"standard"`, `"minimal"`, `"nixpkgs"`, `"security"`, `"off"` |
| `layout` | `Text` | Directory convention: `"flake-parts"`, `"nixpkgs-by-name"`, `"straylight"`, `"nixos-config"`, `"all-flake-module"` — see [Layout Conventions](./layout-conventions.md) |
| `extra-ignores` | `List Text` | Glob patterns for files/directories to skip |
| `overrides` | `List RuleOverride` | Per-rule severity overrides (with an optional documented `reason`) |
| `lsp` | record | Language-server knobs: eval-pool worker count, memory/disk cache quotas |

Remote Dhall imports are refused: the config must be self-contained (a
hostile config file must not phone home on every check).

## Resolution order

A rule's effective severity is resolved in exactly this order:

1. **Your explicit `overrides`** — always win.
2. **The profile chain** — the named profile's rules, child before parent
   (`nixpkgs` inherits from `standard`, `security` from `minimal`).
3. **Built-in defaults** — most rules default on at their listed severity;
   the **opt-in tier** (currently `non-lisp-case`) defaults *off* and fires
   only when a profile or override enables it.

The profile tables live in `config/profiles.dhall` and the rule registry in
`config/rules.dhall`; the executable mirrors are parity-tested against
these files, so the documentation you are reading cannot silently drift
from the binary's behavior.

## Profiles

| profile | inherits | character |
|---|---|---|
| `strict` | — | Full straylight/aleph conventions: everything error, lisp-case enforced. The house profile — narsil's own repo runs it. |
| `standard` | — | The default. Universal bug-catchers on; house-specific rules (prelude wrappers, lisp-case, Dhall templating) off. |
| `minimal` | — | Essential safety checks only, for gradual adoption. |
| `nixpkgs` | `standard` | nixpkgs conventions: `with`/`rec` allowed, derivation metadata strictly required, and **`type-check-failure` remapped to Warning** — the lax mode for code adjacent to known upstream issues. |
| `security` | `minimal` | Injection- and memory-safety focused. |
| `off` | — | Everything ignored (`**/*`). A base for fully custom setups. |

Severity semantics: `Error` fails the run (exit 1), `Warning` reports
without failing, `Info` is informational, `Off` suppresses — in the CLI
*and* the language server, identically. Ignored files (explicit globs or a
profile's) produce no diagnostics anywhere; an explicitly named ignored
file reports `skipped (ignored by config)` rather than silently checking.

## Rule reference

Bash (embedded shell scripts):

| Rule ID | Code | What it catches |
|---|---|---|
| `no-heredoc-in-inline-bash` | ALEPH-B001/B002 | Heredocs and here-strings |
| `no-eval` | ALEPH-B003 | `eval` |
| `no-backtick` | ALEPH-B004 | Backtick command substitution |
| — | ALEPH-B005 | Bare (non-store-path) commands |

Nix:

| Rule ID | Code | What it catches |
|---|---|---|
| `with-lib` | ALEPH-N001 | `with` expressions |
| `rec-anywhere` | ALEPH-N002 | `rec` attrsets |
| `no-substitute-all` | ALEPH-N005 | `substituteAll` |
| `no-raw-mkderivation` | ALEPH-N006 | Raw `mkDerivation` |
| `no-raw-runcommand` | ALEPH-N007 | Raw `runCommand` |
| `no-raw-writeshellapplication` | ALEPH-N008 | Raw `writeShellApplication` |
| `or-null-fallback` | ALEPH-N009 | `or null` implicit fallback |
| `no-translate-attrs-outside-prelude` | ALEPH-N010 | Attribute translation outside the prelude |
| `prefer-write-shell-application` | ALEPH-N011 | `writeShellScript` where an application belongs |
| `long-inline-string` | ALEPH-N012 | Inline strings > 120 chars |
| `missing-meta` | ALEPH-N013 | Derivation without `meta` |
| `missing-description` | ALEPH-N014 | `meta` without `description` |
| `non-lisp-case` | ALEPH-N015 | Non-lisp-case `let` binding (**opt-in**) |
| `missing-class` | — | Module missing its `_class` attribute |
| `default-nix-in-packages` | P001 | Misplaced `default.nix` under `packages/` |
| `type-check-failure` | — | A type inference error (the product) |

Layout findings (`E001`–`E010`) follow the configured convention; see
[Policy Rules](./policy.md) for the full table with examples and
remediation.
