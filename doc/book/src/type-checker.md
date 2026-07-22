# The Type Checker

narsil infers a type for every expression in a Nix file and reports the
places where evaluation *would* fail if the value were demanded. No type
annotations are required or possible — Nix has none — so everything is
inferred, Hindley–Milner style, from the source.

## What it catches

```nix
# a typo'd field on a record it can see
let pkg = { pname = "hello"; version = "2.12"; };
in pkg.pnmae
# error: attribute 'pnmae' missing on closed attribute set (keys: pname, version)

# a wrong-typed argument to a builtin
builtins.stringLength 5
# error: type mismatch: expected String, got Int

# `+` where `++` was meant (a real nixpkgs bug class)
nativeCheckInputs = [ pytest ] + extraInputs;
# error: operator `+` cannot combine [a] and [b]

# a list handed to something that wants a shell string
postBuild = lib.optionalString withManpage [ "make -C docs man" ];
# error: type mismatch: expected String, got [String]

# a module option defined against its declared type
options.services.foo.port = lib.mkOption { type = lib.types.int; };
config.services.foo.port = "8080";
# error: type mismatch: expected Int, got "8080"
```

The last two deserve emphasis. The `optionalString` example is a **latent**
bug: the file evaluates cleanly today because `withManpage` defaults to
`false`, and it breaks — at a user's machine — the day someone flips the
flag. Evaluation cannot see it. The type checker can, and this class of
finding (several confirmed instances in nixpkgs itself) is the reason the
checker exists.

The module example works because narsil treats the NixOS module system's
option declarations as what they are: **a reified type language**. When a
module declares `type = types.listOf types.str`, narsil reads that
declaration and holds the module's own definitions to it — the program is
checked against itself.

## Why you can trust it

A type checker for a dynamic language lives or dies on its false-positive
rate. narsil's is measured, not asserted:

- **The ratchet.** Every change to the engine re-runs against all 43,170
  `.nix` files of a pinned nixpkgs, and the counted error buckets may only
  shrink. The current floor is 28 files — 0.065% — and each of the 28 is
  individually classified (real bug / ledgered idiom / out-of-fragment).
- **The differential oracle.** Stock nixpkgs is well-typed by construction:
  `nix-instantiate` accepts it. Any file narsil rejects is presumed to be
  narsil's bug until a human classifies it otherwise.
- **The mutation ledger.** False-positive pressure pushes checkers toward
  accepting everything. The ledger holds the other direction: 19 `MustCatch`
  entries pin bug shapes the checker must keep rejecting, and 7
  `AcceptedByDesign` entries document each deliberate leniency trade. A
  change that silently loses a catch fails the suite even though no corpus
  number moved.
- **The generative fuzzer.** A type-directed generator produces well-typed-by-
  construction terms and checks that narsil and `nix-instantiate` agree on
  every one — thousands of terms per run, no disagreements.

The details live in [The Apparatus](./testing.md).

## The engine, briefly

Under the hood ([full chapter](./nix-inference.md)): Algorithm-W-style
inference with **row-polymorphic records** (open records accumulate fields
as they flow; closed records reject typos), **union types** for the joins
Nix's dynamism genuinely requires (`if c then x else null`), **occurrence
narrowing** (a branch guarded by `x != null` sees `x` without its null arm
— through `&&`/`||`, `assert`, and the `lib.optionalString cond x` idiom),
**let-polymorphism with SCC dependency analysis** (helpers are polymorphic
across their call sites, in `let` and `rec` alike), and the **module-system
ontology** (declared option types checked against definitions at exact
spans).

Where narsil cannot know, it says nothing: unknown shapes degrade to
dynamic rather than fabricating errors. The boundary between "checked" and
"deliberately not decided" is precise and documented — that is
[The Contract](./contract.md).
