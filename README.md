# // narsil //

> *"The Sword that was Broken shall be reforged."*

Compile-time static analysis for Nix expressions and embedded bash scripts —
a Hindley-Milner type checker, linter, and language server that holds
**99.92% of nixpkgs** (43,170 files) with a corpus-verified false-positive
floor of 28 files, every one of them named and classified
([the contract](./doc/design/contract.md)).

- **Type inference** for bash environment variables -- infers types from `${VAR:-default}` patterns, config assignments, and command usage
- **Hindley-Milner type inference** for Nix expressions -- row polymorphism, type schemes, attrset typing
- **Policy enforcement** -- bans `with`, `rec`, heredocs, `eval`, backticks, and bare (non-store-path) commands
- **Config generation** -- replaces heredoc-templated config with typed `emit-config json|yaml|toml`
- **Scope graphs** -- Visser-style scope graphs for IDE tooling, exportable as JSON or Dhall

## // credit //

narsil is written almost entirely by [Claude](https://claude.ai) (Anthropic) —
the bulk of the engine, the verification apparatus, and the LSP by **Claude
Opus 4.6**, with later rounds (the module-system ontology, the false-positive
endgame, the strictness hierarchy, the LSP debt closure) by **Claude Fable 5** —
working in long driven sessions with a human on the tiller. The commit history
is collapsed for release; this note is the attribution.

## // documentation //

Documentation lives under [`doc/`](./doc/): the mdBook is at [`doc/book/`](./doc/book/)
(built with [mdBook](https://rust-lang.github.io/mdBook/)); design notes, the
specification, and the review/TODO trackers sit alongside it in `doc/`.

```bash
mdbook serve doc/book/    # local preview at http://localhost:3000
mdbook build doc/book/    # build to doc/book/book/
```

Or read the source markdown directly:

- [Introduction](./doc/book/src/introduction.md)
- [Getting Started](./doc/book/src/getting-started.md)
- [CLI Reference](./doc/book/src/cli.md)
- [Architecture](./doc/book/src/architecture.md)
- [Policy Rules](./doc/book/src/policy.md)
- [Specification](./doc/SPECIFICATION.md) · [Hacking](./doc/HACKING.md) · [Design notes](./doc/design/)

## // quick start //

```bash
# Run all checks on a project (auto-detects .sh, .nix, or directory)
nix run github:hypermodern-consulting/narsil -- check ./

# Check a single file
nix run github:hypermodern-consulting/narsil -- check ./default.nix

# Infer types and add annotation comments
nix run github:hypermodern-consulting/narsil -- infer ./default.nix

# Generate typed config emitter
nix run github:hypermodern-consulting/narsil -- emit ./configure.sh

# Show scope graph
nix run github:hypermodern-consulting/narsil -- scope ./default.nix

# Start LSP server
nix run github:hypermodern-consulting/narsil -- lsp
```
