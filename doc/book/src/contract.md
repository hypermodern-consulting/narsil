# The Contract

*What a diagnostic means, and where the fragment ends.*

## Why this chapter exists

The residual error count on stock nixpkgs is small enough to name every file
in it. That invites the wrong summary — "here's where it got hard" — which is
a progress report, not a product claim. This document states the claim: what
a `narsil` diagnostic *asserts*, which programs are *inside* the typed
fragment, and the two named exclusions that bound it. Every residual
diagnostic on stock nixpkgs classifies into exactly one of three buckets
defined here. There is no fourth bucket.

## The judgment: demand-independent typing

A diagnostic asserts:

> **If this expression's value is ever demanded, evaluation fails.**

It does *not* assert "this file fails to evaluate." The distinction is the
whole product. Nix is lazy, so `nix-instantiate` only reports errors on the
thunks a particular configuration happens to force; a type error sitting
behind `withManpage = false` is invisible to evaluation until someone flips
the flag — at a user's machine, at build time. Every typed language makes the
same choice: `if false then 1 + "a"` is a type error in OCaml even though it
cannot run. In Nix, *configuration flips are the `false`*.

Concretely, from the current residual set: `alot` and `fwknop` pass
`lib.optionalString` a list — both files evaluate cleanly today, and both
break the moment their guarding flag flips. An evaluation-as-arbiter design
("suppress any diagnostic in a file `nix-instantiate` accepts") would silence
exactly these. That is why the oracle sweep uses evaluation to *classify
suspected false positives during development* but the checker never uses it
to *veto a diagnostic*. Evaluation enriches the environment (below); it does
not judge the program.

## The fragment: first-order manifest shapes

`narsil` types the fragment of Nix in which every record shape is
**manifest** — readable without evaluating user code. A shape is manifest
when it is:

1. **Source-manifest** — written in the expression: literals, formals,
   `let`/`rec` bindings, `inherit`, `//` of manifest operands. This is the
   HM-with-rows engine: row polymorphism, unions, occurrence narrowing,
   honest defaults, SCC generalization.
2. **Declaration-manifest** — stated in the module system's reified type
   language (`mkOption { type = types.…; }`), which we *read*, never guess
   (the module-system design note in `doc/design/`).
3. **Interface-manifest** — produced by evaluating a *closed, trusted spine*
   once and seeding the result into the environment: the pkgs oracle today,
   the options.json oracle next. Evaluation is welcome here precisely
   because it plays the role of a declaration, not of an arbiter.

Inside this fragment the checker aims at zero false positives on stock
nixpkgs (the ratchet), full stop.

## The two exclusions

Everything the checker declines to decide is one of these, by name.

### E1 — demand-correlated well-typedness

Code whose safety depends on *which thunks are forced*, correlated through
values the type system does not track: sibling-field guards (`enable = elk ?
journalbeat; package = elk.journalbeat`), caller protocols
(`__fromCombineWrapper` implying an argument is never null on the forced
path), sentinel accumulators replaced before use (kubo's `foldl'` with a
`throw` seed), bindings that are simply dead under all configurations
(ruby's test driver).

Under the judgment above these sites are **diagnosed, and correctly so** —
the expression *would* fail if demanded; nixpkgs is relying on it never
being demanded. We do not model demand: static demand analysis for a lazy
untyped language is undecidable in general and unconvincing in the small.
The occurrence-narrowing table covers the *lexical* guard idioms
(`x != null && …`, `optionalString cond …`, `assert`); what it cannot cover
is correlation carried through values across bindings.

The E1 sites in stock nixpkgs are few, enumerable, and tracked in the
accepted-FP ledger. They are facts about nixpkgs, not bugs in the model.

### E2 — eval-time shape computation

Code that *computes record shapes at evaluation time*: `lib.fix` /
`extends` fixpoints whose output shape is the output of the function being
fixed, `typeMerge` folding type values, `mapAttrs` over dynamic names
building option sets, dispatch tables keyed by `builtins.typeOf v`. Here
the shape of an expression is a *runtime value*; typing it honestly is
dependent-types territory, and no HM extension reaches it.

This exclusion is bounded deliberately: the files that live here are the
metaprogramming core of nixpkgs itself (`lib/types.nix`, `lib/modules.nix`,
`lib/attrsets.nix`, …), and their *consumers* never need them typed
structurally — consumers see them through manifest interfaces (exclusion
route 3 above: declarations and oracles). E2 code is where interfaces come
*from*, not where users live.

## The accounting invariant

Every diagnostic the checker emits on stock nixpkgs must classify as exactly
one of:

| bucket | meaning | disposition |
| --- | --- | --- |
| **TP** | a real bug: the expression fails when demanded, and a reachable configuration demands it | upstream PR queue |
| **E1** | demand-correlated site nixpkgs relies on | accepted-FP ledger, enumerated |
| **E2** | eval-time shape computation, out of fragment | named singles in lib/ internals |

The apparatus enforces the boundary in both directions:

* the **ratchet** (`test/oracle/nixpkgs-baseline.json`) pins the total — the
  buckets may only shrink;
* the **mutation ledger** (`test/MutationSpec.hs`) pins the *trades* — every
  deliberate leniency has an `AcceptedByDesign` witness, and every
  strictness kept to protect a TP has a `MustCatch` witness
  (`mut_optionalstring_list_arg`, `mut_getexe_string_arg`,
  `mut_plus_on_lists` guard precisely the TPs in the current residue).

A future change that moves a site between buckets must move its witness. A
change that cannot say which bucket it is trading against does not land.

## The current residue, classified (nixpkgs pin 2cdee5cd…, 28 files)

* **TP (≈10)** — `+` on lists (networkd.nix:560, ale-py), `optionalString`
  given lists (alot, fwknop, chruby, gopher64), the Solaris stdenv
  bootstrap's `shell`/`binutils` vs `runtimeShell`/`bintools`, tex's
  `findLhs2TeXIncludes` called without `lib`, `lib/tests/modules/functionTo/
  wrong-type.nix` (an *intentional* negative test — allowlisting
  `*/tests/modules/*` in the driver is fair game).
* **E1 (7)** — elk, kubo, ruby driver, texlive `build-tex-env`,
  eval-cacheable-options, ncps, plus chruby's cousin shape; all in the
  ledger with their correlation spelled out.
* **E2 (≈11)** — lib/ internals and their nearest neighbors (types.nix,
  modules.nix, attrsets.nix, lists.nix, the lib test suites, znc's typeOf
  dispatch, stage.nix, coq meta-fetch, rust 1_95, stdenv freebsd/native).

## Shipping posture

Strict is the default: the judgment is the product, and TPs are reported
even when today's configuration hides them. Quiet-by-default while upstream
PRs are in flight comes from **suppression by enumeration**: the ratchet
baseline for the pinned nixpkgs doubles as a known-issues pack (file +
signature), versioned with the pin. A pedantic mode ignores the pack;
nothing anywhere suppresses by re-judging. The line never moves silently —
it is drawn here, and the witnesses hold it.
