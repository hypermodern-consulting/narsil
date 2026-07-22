# Getting Started

## Installation

narsil is distributed as a Nix flake. Three ways to run it:

```bash
# Run directly without installing (replace command as needed)
nix run github:hypermodern-consulting/narsil -- check ./

# Build the binary locally
nix build github:hypermodern-consulting/narsil
./result/bin/narsil check ./

# Clone and enter a development shell
git clone https://github.com/hypermodern-consulting/narsil
cd narsil
nix develop    # provides ghc, cabal, hls, hlint, mdbook
cabal build    # build from source
```

The binary is named `narsil`. All following examples assume it's on your `PATH`.

## First run

`check` is the primary command. It auto-detects whether the target is a bash script, a Nix file, or a directory:

```bash
narsil check ./deploy.sh   # bash: lint + type inference + policy
narsil check ./default.nix # nix: lints, typechecks, extracts embedded bash
narsil check ./            # directory: runs the full CI pipeline
```

### Checking a bash script

Create a small script with known issues:

```bash
cat > /tmp/demo.sh << 'SCRIPT'
#!/usr/bin/env bash
set -euo pipefail

config.server.port=$PORT
config.server.host="$HOST"

echo "Server: ${HOST}:${PORT}"
wget -q "http://${HOST}:${PORT}/health"
SCRIPT
```

Run the check:

```
$ narsil check /tmp/demo.sh

error[NARSIL-B005]: bare command not allowed: wget
  --> /tmp/demo.sh:8

  Use an explicit store path for external commands:
    /nix/store/...-pkg/bin/wget

3 error(s) in /tmp/demo.sh
```

The tool caught a bare `wget` call — it has no Nix store path, so it can't be verified as reproducible. The two `$PORT` and `$HOST` references are fine: they come from `config.*` assignments where their types are inferred and defaults tracked.

### Checking a Nix file

```bash
cat > /tmp/demo.nix << 'NIX'
{ lib, writeShellScriptBin }:

rec {
  getVersion = builtins.readFile ./VERSION;

  script = writeShellScriptBin "demo" ''
    echo "Hello, ${getVersion}"
  '';
}
NIX
```

```
$ narsil check /tmp/demo.nix

✗ /tmp/demo.nix ── NIX LINT VIOLATIONS:

  NARSIL-N002: rec attrset
    rec enables non-termination and complicates static analysis.
    Use let bindings instead.

  ✗ /tmp/demo.nix (unsupported construct — type check skipped)

Found 1 shell scripts in /tmp/demo.nix

=== demo ===
  OK

1 total error(s)
```

The `rec` attrset triggers `NARSIL-N002` and blocks type inference for the file. The embedded bash (`writeShellScriptBin "demo"`) was extracted and checked separately — it passed.

### Running type inference on a Nix file

```
$ narsil infer /tmp/demo.nix
Error: ... (parse error, same as check)

# After removing `rec` — the file is type-checked and annotations are added:
$ narsil infer /tmp/demo.nix
{ lib, writeShellScriptBin }:

# :: { getVersion : TString, script : TDerivation }
{
  getVersion = builtins.readFile ./VERSION;
  script = writeShellScriptBin "demo" ''
    echo "Hello, ${getVersion}"
  '';
}
```

The `infer` command runs Hindley-Milner type inference and inserts `# :: Type` annotation comments on each binding. Row-polymorphic attrsets, type schemes, and let-generalisation are all supported.

### Generating an emit-config function

Given a script with `config.*` assignments, `emit` generates a bash function that outputs structured config:

```bash
cat > /tmp/configure.sh << 'SCRIPT'
config.server.port=$PORT
config.server.host="$HOST"
config.server.debug=false
config.server.workers=4
SCRIPT
```

```
$ narsil emit /tmp/configure.sh
emit-config() {
    local fmt="${1:-json}"
    case "$fmt" in
        json)
            printf '{'
            printf '"server":{'
            printf '"port":%s' "${PORT:?PORT is required}"
            printf ',"host":"%s"' "${HOST:?HOST is required}"
            printf ',"debug":false'
            printf ',"workers":4'
            printf '}'
            printf '}\n'
            ;;
        yaml)
            printf 'server:\n'
            printf '  port: %s\n' "${PORT:?PORT is required}"
            printf '  host: "%s"\n' "${HOST:?HOST is required}"
            printf '  debug: false\n'
            printf '  workers: 4\n'
            ;;
        toml)
            printf '[server]\n'
            printf 'port = %s\n' "${PORT:?PORT is required}"
            printf 'host = "%s"\n' "${HOST:?HOST is required}"
            printf 'debug = false\n'
            printf 'workers = 4\n'
            ;;
        *)
            echo "Unknown format: $fmt. Use json, yaml, or toml." >&2
            return 1
            ;;
    esac
}
```

Save the output to a file and source it:

```bash
narsil emit /tmp/configure.sh > /tmp/emit-config.sh
source /tmp/emit-config.sh

PORT=8080 HOST=localhost emit-config json
# {"server":{"port":8080,"host":"localhost","debug":false,"workers":4}}
```

All variable references use `${VAR:?}` guards — if a required variable is unset at runtime, the script fails immediately with a clear message rather than producing malformed output.

### Viewing the scope graph

The `scope` command builds a Visser-style scope graph for IDE tooling:

```
$ narsil scope /tmp/demo.nix
=== Scope Graph ===
File: /tmp/demo.nix
Scopes: 3

Scope 0 (FileScope):
  Declarations:
    demo : TString
  References:
    lib (VarRef)
    writeShellScriptBin (VarRef)
    builtins (VarRef)

Scope 1 (FunctionScope):
  Edges:
    -> 0 (Parent)

Scope 2 (AttrSetScope):
  Declarations:
    getVersion : TString
    script : TDerivation
  Edges:
    -> 1 (Parent)

=== All 3 references resolved ===
```

Machine-readable export formats are available:

```bash
narsil scope --json demo.nix   # JSON for tooling integration
narsil scope --dhall demo.nix  # Dhall for zeitschrift
```

### Starting the language server

```bash
narsil lsp
```

The LSP provides diagnostics (lint violations, type errors), hover types, go-to-definition, find-references, completion, rename, and formatting for Nix files. It also extracts and checks embedded bash inside `writeShellScript*` calls.

Point any LSP client at `narsil lsp` as the server command for the `nix`
language. Full feature list and per-editor setup (Neovim, Helix, VS Code)
in [The Language Server](./lsp.md).

## CI integration

`narsil check <directory>` runs the full pipeline on all `.nix` files recursively (skipping `.git`, `.direnv`, `node_modules`, `.cache`, `.lake`, `result`, `result-lib`, `target`). The CI pipeline has four phases:

1. **Typecheck** — runs type inference on every `.nix` file in parallel (capability-bounded). `with`, `rec`, and dynamic attributes are fully supported; nothing is skipped on the pinned-nixpkgs corpus.
2. **Graph** — builds the module dependency graph from `flake.nix`, checks `import` statements, runs lint and layout convention validation.
3. **Nix** — extracts embedded bash from `writeShellScript`, `writeShellApplication`, `writeShellScriptBin` calls and runs the full bash pipeline on each.
4. **Package** — checks that every `packages/` subdirectory contains a `default.nix`.

Example output:

```
$ narsil check ./

═══════════════════════════════════════════════════════════════════════════════
  narsil ci
  ./
═══════════════════════════════════════════════════════════════════════════════

═══════════════════════════════════════════════════════════════════════════════
  narsil typecheck
  47 files
═══════════════════════════════════════════════════════════════════════════════

✓ lib/nix/default.nix
✓ lib/nix/overlays/default.nix
✗ lib/nix/packages/hello/default.nix ── NIX LINT VIOLATIONS:

  NARSIL-N002: rec attrset
    Use let bindings instead.
...
  ✓  42 passed, 5 skipped, 0 failed

  Graph violations: 0 lint, 2 layout
  lib/nix/modules/flake/gpu-broker.nix: 2 violations

  NARSIL-L003: file kind could not be determined
    Add _class = "nixos" or _class = "flake" to the file.

  NARSIL-L004: _class value "foo" does not match location
    File is in nix/modules/flake/ but has _class = "foo"

═══════════════════════════════════════════════════════════════════════════════
  CI Summary
  47 files scanned
  42 passed, 5 skipped
  2 lint violations
  0 package violations
  0 bash violations
  0 graph failures
═══════════════════════════════════════════════════════════════════════════════

  ALL GREEN
```

### Integrating with GitHub Actions / Buildkite

```yaml
# .github/workflows/narsil.yml
name: narsil
on: [push, pull_request]
jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@v4
      - run: nix run github:hypermodern-consulting/narsil -- check ./
```

For Nix-based CI, add a `checks` entry to your flake:

```nix
# flake.nix
checks.x86_64-linux.narsil =
  pkgs.runCommand "narsil-check" { buildInputs = [narsil]; } ''
    narsil check ${./.}
    touch $out
  '';
```

Or use `nix flake check` after adding the check to your flake's checks output.

The tool exits non-zero on any violation, making it suitable as a direct gate in any CI pipeline.

## Configuration

Create `.narsil.dhall` in your project root. The CLI loads it automatically; you can also pass an explicit path with `--config <file>`.

### Minimal config

```dhall
-- .narsil.dhall
{ profile = "standard"
, extra-ignores = [] : List Text
, overrides = [] : List { id : Text, severity : < Error | Warning | Info | Off >, reason : Optional Text }
}
```

### Choosing a profile

| Profile     | `rec`   | `with lib` | `non-lisp-case` | Use case                        |
|-------------|---------|------------|-----------------|---------------------------------|
| `strict`    | error   | error      | error           | New projects, maximum rigour    |
| `standard`  | warning | error      | off             | Most projects (default)         |
| `minimal`   | off     | warning    | off             | Legacy codebases, gradual adoption |
| `nixpkgs`   | off     | warning    | off             | nixpkgs contributions           |
| `security`  | off     | error      | off             | Security-critical code          |
| `off`       | off     | off        | off             | Custom base (all rules default to off) |

### Customising with overrides

```dhall
let Severity = < Error | Warning | Info | Off >

let RuleOverride = { id : Text, severity : Severity, reason : Optional Text }

let override = \(id : Text) -> \(severity : Severity) -> { id, severity, reason = None Text }

let override-with-reason = \(id : Text) -> \(severity : Severity) -> \(reason : Text) -> { id, severity, reason = Some reason }

in  { profile = "standard"
    , extra-ignores =
      [ "vendor/**"
      , "third-party/**"
      , "generated/**"
      ]
    , overrides =
      [ override "rec-anywhere" Severity.Info
      , override-with-reason "no-substitute-all" Severity.Off "We use envsubst over Dhall"
      ]
    } : { profile : Text
        , extra-ignores : List Text
        , overrides : List RuleOverride
        }
```

Available rule IDs:

| ID                               | Category      | Description                                      |
|----------------------------------|---------------|--------------------------------------------------|
| `rec-anywhere`                   | Nix policy    | Forbid `rec` attrsets                            |
| `with-lib`                       | Nix policy    | Forbid `with lib;`                               |
| `no-substitute-all`              | Nix policy    | Forbid `substituteAll` (prefer Dhall)            |
| `no-raw-mkderivation`            | Nix policy    | Forbid raw `mkDerivation` (prefer prelude)       |
| `no-raw-runcommand`              | Nix policy    | Forbid raw `runCommand` (prefer prelude)         |
| `no-raw-writeshellapplication`   | Nix policy    | Forbid raw `writeShellApplication`               |
| `no-heredoc-in-inline-bash`      | Nix patterns  | Forbid heredocs in inline bash strings           |
| `or-null-fallback`               | Nix patterns  | Require fallback with `or`/`null`                |
| `long-inline-string`             | Nix patterns  | Flag multi-line strings in attribute values      |
| `missing-meta`                   | Nix quality   | Derivation missing `meta` attribute              |
| `missing-description`            | Nix quality   | `meta` missing `description`                     |
| `missing-class`                  | Nix quality   | Module missing `_class` attribute                |
| `non-lisp-case`                  | Nix naming    | Identifier not lisp-case                         |
| `default-nix-in-packages`        | Nix naming    | Package dir missing `default.nix`                |
| `cpp-using-namespace-header`     | C++           | `using namespace` in header                      |
| `cpp-raw-new-delete`             | C++           | Raw `new`/`delete` outside prelude               |
| `cpp-namespace-closing-comment`  | C++           | Namespace closing brace missing comment          |

### Setting a layout convention

Add a `layout` field to your config:

```dhall
{ profile = "standard"
, layout = < straylight | nixpkgs-by-name | flake-parts | nixos-config >
, extra-ignores = [] : List Text
, overrides = [] : List { id : Text, severity : < Error | Warning | Info | Off >, reason : Optional Text }
}
```

Layout conventions are validated during the graph phase of `check <dir>`. Four built-in conventions are available — see [Layout Conventions](./layout-conventions.md) for the full directory mapping for each.

## Layout conventions

A **layout convention** enforces where each module kind lives, what files are called, and how modules are exported. Four conventions ship built-in:

### straylight

`nix/` root, kebab-case throughout, dedicated subdirectories per module kind:

```
flake.nix
nix/
  modules/
    flake/     gpu-broker.nix       # FlakeModule
    nixos/     workstation.nix      # NixOSModule
    home/      tmux.nix             # HomeModule
    darwin/    defaults.nix         # DarwinModule
  packages/
    hello/
      default.nix                   # Package
  overlays/
    mesa.nix                        # Overlay
  lib/
    utils.nix                       # Library
  shells/
    default.nix                     # Shell
```

### nixpkgs-by-name

Nixpkgs `pkgs/by-name` layout. Only package locations are enforced:

```
flake.nix
pkgs/
  by-name/
    he/hello/package.nix            # Package
    ri/ripgrep/package.nix
```

### flake-parts

Flat structure, standard flake-parts conventions:

```
flake.nix
modules/          gpu-broker.nix    # FlakeModule
nixos-modules/    workstation.nix   # NixOSModule
packages/
  hello/
    default.nix                     # Package
overlays/
  mesa.nix                          # Overlay
```

### nixos-config

Loose layout for NixOS system configurations:

```
flake.nix
hosts/            mythoform.nix     # NixOSModule
modules/          gpu-broker.nix    # NixOSModule
users/            b7r6/default.nix  # HomeModule
home/             default.nix       # HomeModule
```

### Module kind detection

narsil determines what kind a file is in three steps (first match wins):

1. **`_class` attribute** — `_class = "nixos"`, `_class = "package"`, etc. Supported values: `flake`, `nixos`, `home`, `homeManager`, `darwin`, `package`, `overlay`, `lib`, `shell`.
2. **Structural heuristics** — `final: prev: { ... }` → overlay; `{ config, lib, pkgs, ... }: { options = ...; config = ...; }` → module; `{ stdenv, ... }: stdenv.mkDerivation { ... }` → package; `{ self, inputs, ... }: { perSystem = ...; }` → flake module.
3. **Filename hints** — `flake.nix` → Flake, `package.nix` → Package, `overlay.nix` → Overlay.

If nothing matches, the kind is `Unknown` and the file passes without check.

## Common workflows

### Adding narsil to an existing project

```bash
# 1. Copy the example config
curl -o .narsil.dhall \
  https://raw.githubusercontent.com/hypermodern-consulting/narsil/main/.narsil.dhall.example

# 2. Adjust the profile and add ignores for vendored code
#    Edit .narsil.dhall — set extra-ignores for vendor/, third-party/, etc.

# 3. Run the full CI check
nix run github:hypermodern-consulting/narsil -- check ./

# 4. Fix violations one at a time, starting with the lowest-hanging fruit
```

### Incrementally adopting policies

1. Start with `profile = "minimal"`. This only enforces the truly dangerous stuff (heredocs, eval).
2. Gradually move to `profile = "standard"`. Fix `with lib;` and `rec` usage.
3. For greenfield code, use `profile = "strict"` with a layout convention.

Use per-rule overrides to temporarily downgrade violations you can't fix yet:

```dhall
overrides =
  [ override "rec-anywhere" Severity.Info    -- track but don't block CI
  , override "non-lisp-case" Severity.Warning  -- start flagging naming
  ]
```

### Replacing heredoc config with emit-config

Heredocs are banned unconditionally (NARSIL-B001). The migration path:

```bash
# Before (will fail narsil check):
cat << EOF > config.json
{
  "port": ${PORT},
  "host": "${HOST}"
}
EOF

# After:
config.server.port=$PORT
config.server.host="$HOST"

# Generate the emit-config function (run once, commit the output):
narsil emit configure.sh > lib/emit-config.sh

# Source it and generate config:
source lib/emit-config.sh
emit-config json > config.json
```

The generated function is self-contained — no dependency on narsil at runtime.

### Using the LSP in a development shell

```nix
# flake.nix devShell
devShells.default = pkgs.mkShell {
  buildInputs = [ narsil ];
  shellHook = ''
    echo "narsil LSP available — configure your editor"
  '';
};
```

For `nvim-lspconfig` without a plugin:

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'nix',
  callback = function()
    vim.lsp.start({
      name = 'narsil',
      cmd = { 'narsil', 'lsp' },
      root_dir = vim.fs.dirname(vim.fs.find({ '.narsil.dhall', 'flake.nix' }, { upward = true })[1] or ''),
    })
  end,
})
```

### Checking embedded bash in Nix derivations

Any Nix file that uses `writeShellScript`, `writeShellScriptBin`, or `writeShellApplication` gets its bash content extracted and fully checked:

```bash
narsil check ./default.nix
```

The output identifies each embedded script by name, shows any policy violations (NARSIL-B*), type errors on environment variables, bare commands, and dynamic command detection. Interpolated Nix expressions that don't resolve to store paths are flagged with a warning.
