# CLI Reference

```
narsil [--config <file.dhall>] <command> [args...]
```

## Options

`--config <file.dhall>`
: Path to a Dhall configuration file. If omitted, `narsil` looks for
  `.narsil.dhall` in the current directory. If no file is found, the
  built-in defaults are used.

**Config schema** (`import`-safe Dhall):

```dhall
let Severity = < SevOff | SevInfo | SevWarning | SevError >

in  { profile : Text
    , overrides :
          List { id : Text, severity : Severity, reason : Optional Text }
    , extra-ignores : List Text
    }
```

| Field            | Description                                                   |
|------------------|---------------------------------------------------------------|
| `profile`        | Profile name included in log output (default: `"standard"`).  |
| `overrides`      | Per-rule severity overrides.  Use `SevOff` to silence a rule. |
| `extra-ignores`  | Additional file-glob patterns to skip during recursive scans. |

Common rule IDs available for `overrides`:

| Rule ID                              | Target                                |
|--------------------------------------|---------------------------------------|
| `no-heredoc-in-inline-bash`          | Heredocs and here-strings in shell    |
| `no-eval`                            | `eval` in shell scripts               |
| `no-backtick`                        | Backtick command substitution         |
| `with-lib`                           | `with` expressions                    |
| `rec-anywhere`                       | Recursive attrset `rec { }`           |
| `no-substitute-all`                  | `substituteAll`                       |
| `no-raw-mkderivation`                | Bare `mkDerivation`                   |
| `no-raw-runcommand`                  | Bare `runCommand`                     |
| `no-raw-writeshellapplication`       | Bare `writeShellApplication`          |
| `prefer-write-shell-application`     | `writeShellScript` instead of app form|
| `long-inline-string`                 | Overly long inline strings            |
| `or-null-fallback`                   | `or null` fallback pattern            |
| `no-translate-attrs-outside-prelude` | Untracked attribute translations      |
| `default-nix-in-packages`            | Missing `default.nix` in package dir  |
| `type-check-failure`                 | Type-check failures                   |

## Exit codes

| Code | Meaning                             |
|------|-------------------------------------|
| 0    | All checks passed, no violations.   |
| 1    | Errors or violations found.         |

If the command argument is unrecognised or missing, the usage summary is
printed and the process exits with code 1.

---

## Commands

### `check <path>`

```
narsil check ./default.nix
narsil check ./deploy.sh
narsil check .
```

Full analysis pipeline.  Behaviour depends on the type of `<path>`:

**Path is a directory** — CI mode.

Recursively discovers every `.nix` file (skipping `.git`, `.direnv`,
`node_modules`, `.cache`, `.lake`, `result`, `result-lib`, `target`, and
any globs in `extra-ignores`).  For each file it runs:
- Nix lint (for `with`, `rec`, raw derivations, etc.)
- Derivation quality lint
- Pattern lint (`or null` fallbacks, attr translations)
- Hindley-Milner type inference (skipped for files using `with`, `rec`,
  or dynamic attribute access—these files are reported as `[UNC]`)

Then, if `flake.nix` is present, additionally:
- Builds the module dependency graph and reports lint/layout violations.
- Extracts every embedded bash script (`writeShellScript`,
  `writeShellScriptBin`, `writeShellApplication`) and runs the full
  shell check pipeline on each.

Finally, runs a package-directory lint (`default.nix` presence check).

Prints a CI summary header and footer with pass / fail / skip /
violation counts.  Exits 0 only when every phase is clean.

**Path is a `.nix` file** — single-file Nix check.

Runs the same Nix lint + type inference pipeline as CI mode, then
extracts and checks every embedded bash script found in the file.

**Path is any other file** (typically `.sh`) — shell script check.

- **Forbidden-construct lint**: heredocs, here-strings, `eval`,
  backticks.  (Violations can be suppressed per-rule via Dhall overrides.)
- **Type inference**: builds constraints from variable assignments and
  checks for unification errors.
- **Config-path validation**: detects conflicting config key prefixes
  (e.g. `config.server` vs `config.server.port`).
- **Bare-command detection**: flags external commands that are not
  referenced by store path (`error[ALEPH-B005]`).
- **Dynamic-command detection**: flags `$VAR` command invocations that
  cannot be statically analysed (`error[ALEPH-B006]`).

Reports a summary line and exits 0 only if there are zero errors.

---

### `fmt <file.nix>`

```
narsil fmt ./default.nix
narsil fmt ./modules/server.nix > formatted.nix
```

Pretty-prints a Nix source file to stdout.  Preserves blank-line
groupings, comment lines, and doc-comment blocks (`/** ... */`).
Output is deterministic for a given input.

Exits 1 on parse or I/O errors.

---

### `infer <file.nix>`

```
narsil infer ./default.nix
```

Runs Hindley-Milner type inference on the Nix file and emits the
source with `# :: Type` annotations inserted on every binding.  If
the expression is a pure lambda (`{lib, ...}: ...`), it will also
annotate the `let`-bound identifiers in the body with their inferred
result types.

Output is written to stdout.  Exits 1 on parse or inference errors.

**Tip**: pipe the output into `narsil fmt` for a clean annotated
source:

```
narsil infer ./default.nix | narsil fmt - > ./default.nix
```

(Note that `fmt` does not read stdin here—pipe through a temporary
file or editor.)

---

### `emit <script.sh>`

```
narsil emit ./configure.sh
narsil emit ./scripts/deploy.sh > /tmp/emitter.sh
```

Parses a bash script, extracts its configuration schema (environment
variables, typed config assignments, store paths), and emits an
`emit-config` bash function to stdout.

The generated function is self-contained and supports three output
formats:

```
emit-config json   # JSON
emit-config yaml   # YAML
emit-config toml   # TOML
```

Type guards are generated for `int` and `bool` typed variables.
Required environment variables that are missing cause the function to
fail with a descriptive error.

Exits 1 on parse or inference errors.

---

### `scope <file.nix>`

```
narsil scope ./default.nix
narsil scope --json ./default.nix  > graph.json
narsil scope --dhall ./default.nix > graph.dhall
```

Builds a scope graph from a Nix file and prints it to stdout.

**Default (text) format** prints a human-readable tree with:

- Scope count and file name header.
- Per-scope sections showing declarations (with types if available),
  references (with ref kind), and outgoing edges (with edge labels).
- A resolution summary: either a count of resolved references or a
  list of unresolved / ambiguous names.

Scope kinds: `FileScope`, `LetScope`, `AttrSetScope`, `RecAttrSetScope`,
`FunctionScope`, `WithScope`.

Edge labels: `Parent`, `Import`, `With`, `Inherit`, `AttrAccess`.

**`--json`** emits the scope graph as a JSON object with the following
structure:

```json
{
  "scopes": [
    {
      "id": 0,
      "declarations": [
        { "name": "...", "span": {...}, "scope": 0, "assocScope": null,
          "type": null, "doc": null }
      ],
      "references": [
        { "name": "...", "span": {...}, "scope": 0, "kind": "VarRef" }
      ],
      "edges": [
        { "source": 1, "target": 0, "label": "Parent" }
      ],
      "kind": "FileScope"
    }
  ],
  "root": 0,
  "file": "default.nix"
}
```

Reference kinds: `VarRef`, `AttrRef`, `InheritRef`, `ImportRef`.

**`--dhall`** emits an equivalent Dhall expression for consumption by
zeitschrift or other Dhall-based tooling.

Exits 1 on parse errors.

---

### `lsp`

```
narsil lsp
```

Starts a Language Server Protocol server over stdio.  The server
provides diagnostics (lint and type errors) for opened Nix files.

Exits 0 on clean shutdown.

---

## Dhall config integration

Create a `.narsil.dhall` file in your project root:

```dhall
{ profile = "my-project"
, overrides =
    [ { id = "with-lib"
      , severity = SevOff
      , reason = Some "Using with pkgs; is our house style"
      }
    , { id = "type-check-failure"
      , severity = SevWarning
      , reason = Some "Type inference is advisory for now"
      }
    ]
, extra-ignores = [ "contrib/**", "legacy/**" ]
}
```

- `profile` appears in log output for traceability.
- `overrides` adjust rule severity.  `SevOff` silences the rule
  entirely; `SevWarning` downgrades errors to warnings (so the exit
  code is unaffected); `SevError` (the implicit default) causes
  `exitFailure`.
- `extra-ignores` uses glob patterns.  A path is ignored if it matches
  any pattern.  The tool also always skips: `.git`, `.direnv`,
  `node_modules`, `.cache`, `.lake`, `result`, `result-lib`, `target`.

To use a non-default config path:

```
narsil --config ./ci/check.dhall check .
```

---

## Troubleshooting

### Corrupted Nix eval cache

If you see mysterious parse or type errors after upgrading nixpkgs or
changing Nix evaluation settings, the Nix flake eval cache may be
stale.  Run:

```
nix flake check --no-eval-cache
```

to force a full re-evaluation, then re-run `narsil`.

### Suppressing violations

Use Dhall overrides in `.narsil.dhall` to control per-rule severity.
Set `SevOff` to silence a rule, or `SevWarning` to keep it as a
diagnostic without failing the exit code.

### Dynamic attr-set or `with` expressions

Files using `with`, `rec { }`, or dynamic attribute access (like
`pkgs.${system}`) cannot be type-checked.  `narsil check` skips
type inference for those files (marking them `[UNC]`), but lint checks
still run.
