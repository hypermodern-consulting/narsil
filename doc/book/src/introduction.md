# narsil

Compile-time static analysis for Nix expressions and embedded bash scripts.

narsil sits in the build pipeline and catches bugs _before_ runtime:

- **Type inference** for bash environment variables -- infers types from `${VAR:-default}` patterns, config assignments, and command usage
- **Hindley-Milner type inference** for Nix expressions -- row polymorphism, type schemes, attrset typing
- **Policy enforcement** -- bans `with`, `rec`, heredocs, `eval`, backticks, and bare (non-store-path) commands
- **Config generation** -- replaces heredoc-templated config files with typed `emit-config json|yaml|toml` functions
- **Scope graphs** -- Visser-style scope graphs for IDE tooling (go-to-definition, find-references), exportable as JSON or Dhall

## Design principles

1. **Fail at build time, not runtime.** Every bash `${VAR}` reference is statically checked. Every command is verified against store paths or a known-builtins allowlist.

2. **No escape hatches.** Forbidden constructs (heredocs, eval, backticks) are banned unconditionally. There is no `# narsil: ignore` directive.

3. **Two type systems, one tool.** Bash gets simple first-order types (`TInt`, `TString`, `TBool`, `TPath`). Nix gets full Hindley-Milner with row polymorphism. They meet at the `nix` command: embedded bash is extracted from Nix expressions (`writeShellScript`, `writeShellApplication`, etc.), both are linted and type-checked, and violations from either language are reported together.

4. **Conservative by default.** Unknown commands produce no type constraints. Unsupported Nix constructs (`with`, `rec`, dynamic attrs) cause the file to be skipped rather than producing wrong results.

## Project status

611 tests — QuickCheck properties, a differential oracle against `nix-instantiate`, a mutation ledger, and a generative well-typed fuzzer — plus a working LSP server. The bash analysis pipeline, Nix type inference, scope graph resolution, emit-config generation, LSP server (diagnostics, hover, go-to-definition, completion, references, rename, formatting), and policy enforcement are all tested and working. See [Testing](./testing.md) for the full breakdown.
