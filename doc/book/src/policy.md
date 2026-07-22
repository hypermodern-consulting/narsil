# Policy Rules

narsil performs static analysis on Nix and bash code, enforcing a set of policy rules. Every violation has an error code prefixed `ALEPH-`. Rules are configured via a Dhall file (see [Configuration](#configuration)).

---

## Bash rules

Bash rules (prefix `ALEPH-B`) are enforced on all inline bash scripts found within Nix files: `runCommand`, `writeShellApplication`, `writeShellScript`, and similar derivations.

### ALEPH-B001: Heredocs

```
Forbidden: heredoc (<<, <<-)
```

Heredocs contain interpolations that cannot be statically analyzed. The content is opaque to the parser, making it impossible to verify shell correctness, detect bare commands, or check environment variable usage.

**Rule ID:** `no-heredoc-in-inline-bash`

**Use instead:**
- `emit-config` for structured output (JSON, YAML, TOML)
- `printf` for short formatted strings
- Generate content in Nix and reference via `pkgs.writeText`:
  ```bash
  cat ${pkgs.writeText "msg" ''...''}
  ```

### ALEPH-B002: Here-strings

```
Forbidden: here-string (<<<)
```

Here-strings share the same problem as heredocs: interpolation inside `<<<` is not statically analyzable. The parser cannot see what data flows into commands.

**Rule ID:** `no-heredoc-in-inline-bash` (shared with B001)

**Use instead:**
```bash
echo "string" | command          # or:
printf '%s' "string" | command
```

### ALEPH-B003: eval

```
Forbidden: eval (including builtin eval, command eval)
```

Dynamic code execution defeats static analysis entirely. `eval` concatenates arbitrary strings and executes them, making it impossible to verify that the resulting code is safe, reproducible, or free of bare external commands.

**Rule ID:** `no-eval`

**Use instead:**
```bash
declare "$name=$value"          # dynamic variable assignment

case "$mode" in                 # command dispatch
  a) /nix/store/...tool/bin/tool ... ;;
  b) /nix/store/...other/bin/other ... ;;
esac
```

### ALEPH-B004: Backticks

```
Forbidden: backtick command substitution (`cmd`)
```

Backticks are deprecated POSIX syntax with broken nesting semantics. Nesting requires backslash-escaping internal backticks, which is error-prone and unreadable.

**Rule ID:** `no-backtick`

**Use instead:**
```bash
result=$(command)
```

### ALEPH-B005: Bare commands

```
error[ALEPH-B005]: bare command not allowed
```

Commands that are neither `/nix/store/...` paths nor shell builtins are rejected. This ensures reproducibility — every external tool must come from a Nix derivation, with its full store path visible in the source.

Shell builtins (`echo`, `printf`, `test`, `[`, `set`, `export`, `declare`, `local`, `read`, `cd`, `pwd`, `true`, `false`, `:`) are always permitted.

**Not configurable.** Bare commands are always errors — they represent real missing-dependency bugs that cannot be safely suppressed.

### ALEPH-B006: Dynamic commands

```
error[ALEPH-B006]: dynamic command not allowed
```

Commands invoked via variable expansion (`$CMD arg1 arg2`) cannot be statically verified. The analyzer cannot determine which binary will execute, so it cannot check store-path correctness.

**Not configurable.** Dynamic commands are always errors — they bypass the entire static analysis pipeline.

**Use instead:**
```bash
case "$mode" in
  a) /nix/store/...-tool/bin/tool arg1 arg2 ;;
  b) /nix/store/...-other/bin/other arg1 arg2 ;;
esac
```

---

## Nix rules

Nix rules (prefix `ALEPH-N`) are enforced on all `.nix` source files.

### ALEPH-N001: `with` expressions

```
ALEPH-N001: `with` expression
```

`with` obscures scope, breaks go-to-definition, creates shadowing hazards, and makes type inference unsound. A single `with` silently changes the meaning of every unbound variable in its body — a deeply non-local effect that defeats human readers and tooling alike.

**Rule ID:** `with-lib`

**Use instead:**
```nix
inherit (expr) name1 name2;
```

### ALEPH-N002: `rec` attrsets

```
ALEPH-N002: `rec` attrset
```

`rec` enables infinite loops (non-termination), complicates static analysis, makes evaluation order-dependent, and breaks referential transparency. A `rec` attrset is a miniature fixed-point that the type checker cannot reason about.

**Rule ID:** `rec-anywhere`

**Use instead:**
```nix
let
  x = ...;
  y = ... x ...;
in { inherit x y; }
```

### ALEPH-N005: `substituteAll`

```
ALEPH-N005: `substituteAll`
```

`substituteAll` copies all derivation dependencies into the store for a single-variable substitution. It is needlessly expensive and creates opaque runtime behavior that obfuscates data flow.

**Rule ID:** `no-substitute-all`

**Use instead:** `substituteInPlace` or `substitute` with explicit values.

### ALEPH-N006: raw `mkDerivation`

```
ALEPH-N006: raw `mkDerivation`
```

Direct `mkDerivation` calls bypass language-specific wrappers that provide important build phases, hooks, and type-safe paths. Raw derivations miss guardrails that prevent common packaging mistakes.

**Rule ID:** `no-raw-mkderivation`

**Use instead:** Language-specific wrappers (`stdenv.mkDerivation`, `buildPythonPackage`, etc.) or project-level prelude wrappers.

### ALEPH-N007: raw `runCommand`

```
ALEPH-N007: raw `runCommand`
```

Direct `runCommand` calls create derivations without proper package metadata and bypass build system conventions. These ad-hoc derivations lack the structure that makes packages discoverable and maintainable.

**Rule ID:** `no-raw-runcommand`

**Use instead:** `runCommandWith` or a proper derivation wrapper.

### ALEPH-N008: raw `writeShellApplication`

```
ALEPH-N008: raw `writeShellApplication`
```

Direct `writeShellApplication` calls bypass narsil's shell script linting and type checking. Scripts should be declared through the module system so they receive automatic validation, shellcheck integration, and metadata enforcement.

**Rule ID:** `no-raw-writeshellapplication`

**Use instead:** The narsil module system wrapper (e.g., `aleph.shell.writeShellApplication`).

### ALEPH-N009: `or null` fallback

```
ALEPH-N009: `or null` fallback
```

Implicit `or null` fallbacks silently swallow attribute errors, masking real bugs when expected fields are missing. Null values propagate through the program and surface as confusing errors far from the source.

**Rule ID:** `or-null-fallback`

**Use instead:**
```nix
# Before:
x.y or null

# After — explicit, auditable:
if x ? y then x.y else null
```

### ALEPH-N010: Attribute translation calls

```
ALEPH-N010: attribute translation call
```

`translateAttrs`, `mapAttrsToList`, and `mapAttrsFlatten` circumvent the type system by dynamically reshaping attribute sets. These functions should only be used in designated prelude directories where translation logic is centralized and reviewable.

**Rule ID:** `no-translate-attrs-outside-prelude`

**Use instead:** Move translation logic to `lib/prelude/` or use statically known attribute sets.

### ALEPH-N011: `writeShellScript`

```
ALEPH-N011: `writeShellScript`
```

`writeShellScript` and `writeShellScriptBin` create shell scripts without runtime metadata: no declared name, runtime inputs, or description. They also bypass shellcheck and the `-euo pipefail` safety flags.

**Rule ID:** `prefer-write-shell-application`

**Use instead:** `writeShellApplication`, which requires explicit metadata and enables shell linting.

### ALEPH-N012: Long inline strings

```
ALEPH-N012: long inline string (N chars)
```

Inline strings longer than 120 characters clutter source files, make diffs hard to read, and should be extracted to separate files for reviewability and reuse.

**Rule ID:** `long-inline-string`

**Use instead:**
```nix
builtins.readFile ./data.txt
```

### ALEPH-N015: Non-lisp-case binding

```
ALEPH-N015: non-lisp-case binding `myThing`
```

Author-chosen names — `let` bindings — use lowercase-with-dashes in
straylight code. Attribute keys mirror external schemas (`buildInputs`,
`perSystem`) and lambda formals are caller-dictated, so only `let` bindings
are checked; trailing primes (`x'`) are allowed. **Opt-in**: this rule is
off by default and under every profile except `strict` (or an explicit
override). In the editor, the finding carries a complete rename quickfix —
declaration plus every reference.

**Rule ID:** `non-lisp-case`

**Use instead:**
```nix
let my-thing = 1; in my-thing
```

### ALEPH-N013: Missing `meta`

```
ALEPH-N013: missing `meta`
```

Derivations should include a `meta` attribute with package metadata for discoverability. Without meta, packages are invisible to search tools and downstream consumers.

**Rule ID:** `missing-meta`

**Use instead:**
```nix
meta = with lib; {
  description = "...";
  license = licenses.mit;
  platforms = platforms.all;
};
```

### ALEPH-N014: Missing `description` in meta

```
ALEPH-N014: missing `description` in meta
```

The `meta` attribute must contain a `description` field providing human-readable documentation of the package's purpose. Descriptions are essential for package search and audit trails.

**Rule ID:** `missing-description`

**Use instead:**
```nix
meta = {
  description = "A short summary of what this package does";
};
```

---

## Layout rules

Layout rules (codes `E001`–`E010`) validate file placement and naming
against the project's configured [layout convention](./layout-conventions.md)
(`layout = "flake-parts"`, `"nixpkgs-by-name"`, `"straylight"`, …). They run
in directory mode (`narsil check .`) and each finding names the expected
location or name.

| Code | Condition |
| --- | --- |
| E001 | File in wrong location for its module kind |
| E002 | File in forbidden location |
| E003 | Wrong file name convention |
| E004 | Wrong attribute name convention |
| E005 | Wrong identifier convention |
| E006 | Must be a flake module but isn't |
| E007 | `_index.nix` files are banned |
| E008 | `_main.nix` files are banned |
| E009 | Missing required `_class` attribute |
| E010 | `_class` value doesn't match location |

```
error[E001]: File in wrong location for NixOSModule
 --> stray-module.nix:1:1
  = help: expected: modules/nixos/... or nixos-modules/...
```

## Package rules

Package rules (prefix `ALEPH-P`) validate package directory structure.

### ALEPH-P001: Missing `default.nix`

```
ALEPH-P001: Package directory must contain a `default.nix` file
```

Every directory classified as a package must contain a `default.nix` entry point. This ensures consistent structure and makes packages discoverable by tooling.

**Rule ID:** `default-nix-in-packages`

---

## Type check rule

### Type inference failure

When narsil's type inference finds an error, the finding is governed like
any other rule. Default severity is **Error**; the `nixpkgs` profile remaps
it to **Warning** (report but pass — the lax shipping mode); `Off`
suppresses it entirely. The same severity applies in the editor.

**Rule ID:** `type-check-failure`

---

## Configuration

narsil looks for a `.narsil.dhall` file in the project root. The configuration has three fields:

| Field           | Type                | Description                                         |
| --------------- | ------------------- | --------------------------------------------------- |
| `profile`       | `Text`              | Profile name (e.g., `"standard"`, `"strict"`)       |
| `extra-ignores` | `List Text`         | Glob patterns for files/directories to skip         |
| `overrides`     | `List RuleOverride` | Per-rule severity adjustments                       |

### Severity levels

| Severity  | Dhall value | Behavior                                              |
| --------- | ----------- | ----------------------------------------------------- |
| `Error`   | `Error`     | Violation is reported and causes exit code 1          |
| `Warning` | `Warning`   | Violation is reported but does not fail the build     |
| `Info`    | `Info`      | Violation is reported as informational only           |
| `Off`     | `Off`       | Rule is completely suppressed                         |

### Rule overrides

A `RuleOverride` has three fields:

| Field      | Type            | Description                       |
| ---------- | --------------- | --------------------------------- |
| `id`       | `Text`          | Rule identifier (see table below) |
| `severity` | `Severity`      | Desired severity level            |
| `reason`   | `Optional Text` | Justification for the override    |

### Example configuration

```dhall
let Severity = < Off | Info | Warning | Error >

in  { profile = "standard"
    , extra-ignores = [ "test/fixtures/**", "legacy/**" ]
    , overrides =
      [ { id = "long-inline-string"
        , severity = Severity.Off
        , reason = Some "Project has generated Nix files with long strings"
        }
      , { id = "rec-anywhere"
        , severity = Severity.Warning
        , reason = Some "Legacy code uses rec; plan to refactor"
        }
      , { id = "or-null-fallback"
        , severity = Severity.Info
        , reason = None Text
        }
      ]
    }
```

### Rule ID reference

All suppressible rules and their identifiers:

| Error code  | Rule ID                              | Description                          |
| ----------- | ------------------------------------ | ------------------------------------ |
| ALEPH-B001  | `no-heredoc-in-inline-bash`          | Heredoc in inline bash               |
| ALEPH-B002  | `no-heredoc-in-inline-bash`          | Here-string in inline bash           |
| ALEPH-B003  | `no-eval`                            | `eval` usage                         |
| ALEPH-B004  | `no-backtick`                        | Backtick command substitution        |
| ALEPH-N001  | `with-lib`                           | `with` expression                    |
| ALEPH-N002  | `rec-anywhere`                       | `rec` attrset                        |
| ALEPH-N005  | `no-substitute-all`                  | `substituteAll` usage                |
| ALEPH-N006  | `no-raw-mkderivation`                | Raw `mkDerivation` call              |
| ALEPH-N007  | `no-raw-runcommand`                  | Raw `runCommand` call                |
| ALEPH-N008  | `no-raw-writeshellapplication`       | Raw `writeShellApplication` call     |
| ALEPH-N009  | `or-null-fallback`                   | `or null` implicit fallback          |
| ALEPH-N010  | `no-translate-attrs-outside-prelude` | Attribute translation outside prelude|
| ALEPH-N011  | `prefer-write-shell-application`     | `writeShellScript` instead of app    |
| ALEPH-N012  | `long-inline-string`                 | Inline string > 120 chars            |
| ALEPH-N013  | `missing-meta`                       | Derivation missing `meta`            |
| ALEPH-N014  | `missing-description`                | `meta` missing `description`         |
| ALEPH-N015  | `non-lisp-case`                      | Non-lisp-case `let` binding (opt-in) |
| ALEPH-P001  | `default-nix-in-packages`            | Package directory without default.nix|
| —           | `type-check-failure`                 | Type inference error                 |

Rules **ALEPH-B005** (bare commands) and **ALEPH-B006** (dynamic commands) cannot be suppressed — they represent fundamental safety properties of the analysis system.

Layout rules (`E001`–`E010`) are governed by the configured layout
convention rather than per-rule severities.

### File ignores

The `extra-ignores` field accepts glob patterns. Patterns are matched against normalized file paths. Examples:

```dhall
{ extra-ignores =
  [ "**/test/**"      # skip test directories
  , "legacy/*.nix"    # skip legacy Nix files
  , "vendor/**"       # skip vendored code
  ]
}
```

Files matching any ignore pattern are excluded from all lint checks and type analysis.
