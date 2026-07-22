# Nix Type Inference

The engine lives under `lib/Narsil/Inference/` — `Nix.hs` (the inference
core), `Nix/Unify.hs` (unification and joins), `Nix/Type.hs` (the type
vocabulary), `Nix/Scheme.hs` (let-polymorphism), `Nix/Builtins.hs` and
`Nix/Lib.hs` (the signature tables), `Nix/Module.hs` (the module-system
ontology), `Nix/Constraint.hs` (the inference monad). It is pure
State+Except — no IO — which is what lets the oracle replay it.

## The type language

```
NixType ::= TVar α                    -- unification variable
          | TInt | TFloat | TBool | TString | TStrLit "…" | TPath | TNull
          | TDerivation
          | TList τ
          | TRec fields row           -- records: fields + row tail
          | TFun τ τ
          | TUnion [τ]                -- untagged union (joins)
          | TAny                      -- deliberate dynamic (top)
```

Records carry a **row tail**: `RClosed` (exactly these fields — a typo'd
select is an error) or `ROpen ρ` (at least these fields; the row variable
accumulates more as the record flows). Row polymorphism in the
Wand/Rémy/Leijen tradition, with Gaster–Jones *lacks* constraints enforced
when row variables bind. Polymorphism lives one level up in `Scheme`
(`∀ vars. τ`), instantiated fresh per use site with a one-step rename.

## The load-bearing decisions

**Unify asserts, merge joins.** `unify` makes two types equal;
`mergeTypes` computes a least upper bound (the arms of an `if`, the
elements of a list). Keeping these distinct is where most false positives
die: a *join* must never rigidify a still-free variable to its concrete
sibling — `imports = [ ./hw.nix extraConfig ]` is heterogeneous on
purpose — so joins produce unions where unification would have bound.

**Occurrence narrowing.** A branch guarded by `x != null` sees `x` with
its null arm removed; predicates (`isString x`, `lib.isDerivation x`),
`assert`, the lazy right operands of `&&`/`||`, `x.attr or null != null`
probes, and the guarded-combinator idiom (`lib.optionalString (x != null)
x`) all narrow. Narrowing is branch-local *shadowing*, never unification —
nothing leaks out of the branch.

**Honest defaults.** `{ python ? null }:` types the formal `Null | α` (the
placeholder-sentinel idiom), `? { }` is an anonymous open record
(callPackage's placeholder), `? false` is `Bool | α` (the flag sentinel).
A caller *replaces* a default rather than having to agree with it.

**The occurs check guards the substitution, not the program.** Lazy Nix
makes self-referential values legal (`let x = [ x ];` — the fixpoint APIs
live on this); an equirecursive binding leaves the variable unconstrained
instead of manufacturing an "infinite type" error.

**Laziness-shaped operators.** `==`/`!=` are total and never error; `<` is
polymorphic and non-binding; `+` resolves over a lattice (Int/Float
infection, String/Path concatenation, derivation coercion via the
`outPath`/`__toString` witness) without forcing unlike operands to unify;
`//` has a real dispatch table rather than an equality fallback.

**SCC let-polymorphism, everywhere.** Bindings — in `let` *and* `rec` —
are grouped into strongly-connected components, inferred in dependency
order, and generalized between groups, so one helper serves differently-
shaped call sites.

## The module-system ontology

The NixOS module system carries a reified type language, and narsil reads
it rather than guessing: `mkOption { type = types.listOf types.str; }`
*declares* `[String]`. Declarations build an option tree; the `config`
parameter binds to the declared spine (so `cfg.port` is `Int`, precisely);
and the same file's definitions are checked against the declarations at
their exact source spans. `mkIf`/`mkDefault`/`mkMerge` are modeled at their
honest types (priority wrappers return wrapper shapes; `mkMerge` takes
heterogeneous fragments). The full `types.*` mapping and design rationale
live in `doc/design/module-system.md`.

## Interfaces by evaluation

Where shapes cannot be read from source, narsil *evaluates trusted spines
once* and seeds the results into the environment: the nixpkgs package
oracle (`pkgs.<attr>` resolves to a real shape, so a typo'd attribute is a
real missing-attribute error) and the cross-module closure (imports and
`callPackage ./path` resolve to the target file's inferred type — eval-free,
via the shared edge scanner). Evaluation enriches the environment; it never
vetoes a diagnostic — see [The Contract](./contract.md) for why that
direction matters.

## What degradation looks like

Where narsil cannot know, it says nothing: unresolved `callPackage` results
are per-site opaque, dynamic attribute keys make a record open rather than
wrong, unknown `lib` functions get permissive schemes. The engine's
disposition is *asymmetric*: precision is spent where it can be verified
against the corpus, and dynamism is admitted honestly everywhere else. The
measured result of that asymmetry is the 0.065% false-positive floor
described in [The Apparatus](./testing.md).
