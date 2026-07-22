# nixpkgs symbol & type completion — design + spike findings

Goal: complete the **symbols inside a package** (`pkgs.hello.<TAB>` → `meta`,
`override`, `pname`, …) and ultimately their **types**, fast, staying as far
from real evaluation as possible.

## The tier stack

Resolution of `pkgs.<pkg>.<attr>` is layered, fast-first:

| tier | source | cost | gives | coverage |
|---|---|---|---|---|
| **1 — shape template** | the ~40 attrs every `mkDerivation` output carries (`override`, `overrideAttrs`, `meta`, `outPath`, `drvPath`, `pname`, `version`, `passthru`, …) | constant, no eval | names | every derivation |
| **2.5 — hnix spine-force** | apply `package.nix` to a *mocked* environment, force the output attrset spine, read the keys | ~4ms/pkg | names | ~87% of by-name |
| **2 — inference** | HM row-type inference over the package source | per-file, no eval | names + **types** | where inference converges |
| **3 — `nix eval` (C++)** | real evaluation for the dynamic residue | slow | real values/types | everything, last resort |

Names come cheaply from tier 1 + 2.5; **types** are what tier 2 (inference) is
for. Tier 3 is C++ nix only — see the spike result on hnix below.

## Spike findings (hnix evaluator)

The premise was "we have hnix, evaluate cleanly in a thread pool." Two questions
were settled empirically against a real nixpkgs checkout.

**hnix cannot evaluate real nixpkgs.** `import <nixpkgs> {}` fails at
`nixpkgs/default.nix`'s feature gate: *"requires … `builtins.nixVersion` reports
at least 2.18 … You are evaluating with Nix 2.3."* So the "amortize the real
fixpoint once, spine-force packages against it" architecture is **dead via
hnix** — real evaluation means the C++ `nix` CLI (the startup cost we wanted to
avoid), or embedding libnixexpr. Inference is therefore not just elegant; for
the no-C++-nix path it is the *only* route to types.

**Mock-fed spine-forcing works — cheaply and generically.** Bypassing
`default.nix` by importing `package.nix` directly, auto-mocking its formals
(`builtins.functionArgs`), and supplying a universal fix-style builder mock,
`builtins.attrNames (pkg mockArgs)` returns the package's declared attrs + the
shape template **without forcing any value** — `src = fetchurl {…}`,
`patches = lib.optional …`, `meta = { license = lib.licenses.X; }` all stay
lazy, because `attrNames` forces only the spine (the keys). The lazy-spine
property is the whole trick.

- **~87% hit on random by-name packages, ~4ms each.** The miss (~13%) is the
  long tail (exotic builders, or a package that forces something in its spine);
  tier 1 backstops it.
- The mock is tiny: every formal is a callable attrset (`__functor`) exposing
  the common builders (`mkDerivation`, `buildGoModule`, `rustPlatform.build*`,
  `python3Packages.buildPython*`, `buildNpmPackage`, …) as one fix-style
  function. Builder coverage is the only knob — widen the list, widen the hit.
- It gives **names, not types**: `mapAttrs (_: typeOf)` would force the values
  and pull in the real `fetchurl`/`lib`/`stdenv`, breaking the mock. So names
  via spine-force, types via inference — they compose.

### The spine-forcer (validated)

```nix
pkgPath:
let
  pkg = import pkgPath;
  shape = {
    type = "derivation"; outPath = "x"; drvPath = "x"; name = "x";
    override = null; overrideAttrs = null; overrideDerivation = null;
  };
  mk = f: let s = (if builtins.isFunction f then f s else f) // shape; in s;
  builders = [ "mkDerivation" "buildPythonPackage" "buildPythonApplication"
    "buildRustPackage" "buildGoModule" "buildNpmPackage" "buildDunePackage"
    "buildPerlPackage" "buildDotnetModule" "stdenvNoCC" /* … widen for coverage */ ];
  ns = builtins.listToAttrs (map (n: { name = n; value = mk; }) builders);
  mockArg = ns // { __functor = _: f: mk f; };
in builtins.attrNames (pkg (builtins.mapAttrs (_: _: mockArg) (builtins.functionArgs pkg)))
```

## Consequences

- **Spine-forcing is the workhorse for symbol *names*** — 4ms, 87%+, a tiny
  mock — better than modelling callPackage/mkDerivation in inference just to get
  names. Run it in the worker pool; an overnight name-index of all ~20k packages
  is minutes, not hours.
- **Types stay with inference (tier 2).** That's the harder core and the real
  research, now clearly scoped to *types*, not names.
- **Real eval (tier 3) is C++ nix, used rarely**, for genuinely dynamic values
  the above can't give — content-address and persist its results hard.
- Background indexing, content-addressed per-package cache (keyed by
  `package.nix` content, not nixpkgs rev), NVMe persistence, and memory/disk
  ceilings are unchanged by the spike.
