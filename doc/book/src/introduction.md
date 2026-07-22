# narsil

> *"The Sword that was Broken shall be reforged."*

narsil is a compile-time type checker, linter, and language server for the
Nix expression language (and the bash embedded in it). It finds bugs before
evaluation does — including the bugs evaluation *can't* find, because
laziness hides them behind configuration flags nobody has flipped yet.

## The numbers

Claims about static analysis for Nix are cheap; corpus verification is not.
narsil's engine is held to a ratcheted differential baseline against all of
nixpkgs:

| metric | value |
| --- | --- |
| nixpkgs files checked | **43,170** (every `.nix` file at the pinned revision) |
| well-typed | **43,134 (99.92%)** |
| residual diagnostics | **28 files (0.065%)** — every one classified |
| crashes / hangs / timeouts | **0 / 0 / 0** |
| test suite | **611 tests**, six suites |

The 28 residuals are not a mystery pile: roughly ten are **real bugs in
nixpkgs** (confirmed by hand — `+` where `++` was meant, `optionalString`
fed lists, a builtin that doesn't exist), seven are ledgered idioms whose
safety depends on laziness the type system deliberately does not model, and
the rest are the metaprogramming core of `lib/` itself. The precise
accounting — what a diagnostic *asserts*, what the checker deliberately
declines to decide — is written down in [The Contract](./contract.md).

## What it does

- **Hindley–Milner type inference** for Nix — row-polymorphic records, union
  types, occurrence narrowing, let-polymorphism, and a typed model of the
  NixOS module system that *reads* `mkOption { type = types.…; }`
  declarations instead of guessing.
- **A language server** — type errors as squiggles, inferred-type inlay
  hints that survive errors, scope-aware completion, go-to-definition,
  rename with full-reference edits, document outline, and eval-backed
  `pkgs.…` completion.
- **Policy enforcement** — a profile-governed rule set (`with`, `rec`,
  heredocs, naming conventions, derivation hygiene) shared letter-for-letter
  between the CLI and the editor.
- **Bash analysis** — type inference for environment variables in embedded
  shell scripts, plus injection-shaped lint rules.
- **Layout conventions** — directory-structure enforcement for flake-parts,
  nixpkgs-by-name, and other project shapes.
- **Typed config generation** — `emit-config` replaces heredoc-templated
  config files with schema-checked emitters.

## Where to start

[Getting Started](./getting-started.md) installs it and runs the first
check. [The Type Checker](./type-checker.md) shows what it catches and why
you can trust it. [The Language Server](./lsp.md) wires it into your
editor. If you want to know exactly where the boundary of the analysis
lies — the honest answer, adversarially stated — read
[The Contract](./contract.md).

narsil is written almost entirely by Claude (Anthropic); see
[Credits](./credits.md).
