# The Language Server

`narsil lsp` speaks the Language Server Protocol over stdio. Everything the
CLI knows, the editor knows: the same inference engine, the same rule set,
the same profile-governed severities — the editor and the command line
disagree about nothing.

## Features

| capability | what you get |
| --- | --- |
| **Diagnostics** | Type errors as squiggles (the flagship — at a corpus-verified 0.065% false-positive floor), plus all lint families, published on open/change/save |
| **Inlay hints** | Inferred types after each binding — and one type error does not blank the file: hints for everything typed before the error survive |
| **Completion** | Names lexically in scope at the cursor (from the AST spine), builtins with their type schemes, module options — all prefix-filtered; `pkgs.…` completes package names and *package attributes* via an eval-backed warm cache |
| **Hover** | Inferred types; module option docs |
| **Go to definition / references / rename** | Scope-graph navigation; works with the cursor on a *use or the declaration*; rename edits every reference |
| **Document symbols** | Outline of attrset and `let` bindings, classified by value shape |
| **Signature help** | Builtin signatures rendered from their type schemes |
| **Code actions** | Quickfixes keyed to rule codes; a non-lisp-case finding carries a complete rename (declaration + every reference) as a ready-to-apply edit |
| **Semantic tokens** | Full-document token classification |

Severity follows your config: with `profile = "nixpkgs"`, type errors
publish as warnings (the lax shipping mode); an explicit `Off` silences a
rule in the editor exactly as it does in CI. Files matched by ignore globs
(including the `off` profile's ignore-everything) publish no diagnostics.

## Editor setup

### Neovim (nvim-lspconfig)

```lua
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.narsil then
  configs.narsil = {
    default_config = {
      cmd = { "narsil", "lsp" },
      filetypes = { "nix" },
      root_dir = lspconfig.util.root_pattern(
        ".narsil.dhall", "flake.nix", ".git"),
    },
  }
end

lspconfig.narsil.setup({})
```

### Helix

```toml
# ~/.config/helix/languages.toml
[language-server.narsil]
command = "narsil"
args = ["lsp"]

[[language]]
name = "nix"
language-servers = ["narsil"]
```

### VS Code / other clients

Any generic LSP client works: launch `narsil lsp` over stdio for the `nix`
language. A dedicated VS Code extension is planned; until then, extensions
that let you register an arbitrary language server (e.g. *Generic LSP
Client*) do the job.

## Performance notes

The server never blocks a request on a build: cross-module environments
come from a project cache filled by background workers (content-hashed,
reverse-dependency invalidation), and cold caches answer from the current
file alone with cross-file precision arriving on later requests. The
nixpkgs completion backend keeps a warm pool of evaluator workers whose
size and memory/disk quotas are set in [Configuration](./configuration.md)
(`lsp.max-threads`, `lsp.max-memory-mb`, `lsp.max-disk-mb`).
