# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
#     "He'd operated on an almost permanent adrenaline high, a byproduct of
#      youth and proficiency, jacked into a custom cyberspace deck."
#
#                                                                   — Neuromancer
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                                      // config
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Dhall configuration schema for `narsil` lint rules.

# ══════════════════════════════════════════════════════════════════════════════
#                                                                 // quick start
# ══════════════════════════════════════════════════════════════════════════════

Create `.narsil.dhall` in your project root:

```dhall
let Narsil = https://raw.githubusercontent.com/.../config/package.dhall

in  Narsil.Config::{
    , profile = "standard"
    }
```

Or copy `.narsil.dhall.example` from this repo.


# ══════════════════════════════════════════════════════════════════════════════
#                                                                    // profiles
# ══════════════════════════════════════════════════════════════════════════════

| Profile | Use Case | `non-lisp-case` | `rec` | `with lib` |
|---------|----------|-----------------|-------|------------|
| `strict` | New straylight projects | error | error | error |
| `standard` | Most projects | off | warning | error |
| `minimal` | Legacy codebases | off | off | warning |
| `nixpkgs` | nixpkgs contributions | off | off | warning |
| `security` | Security-critical code | off | off | error |
| `off` | Custom base | off | off | off |


## // strict

Full straylight conventions. Enforces:

- Lisp-case identifiers (forces code through prelude)
- Dhall for all text templating
- Prelude wrappers for derivations
- No `rec`, no `with`

Use for new straylight projects or projects that want maximum rigor.


## // standard

Sensible defaults. Enforces:

- No `with lib;`
- No heredocs in inline bash
- Best practices as warnings

Does NOT enforce:

- Lisp-case identifiers
- Dhall templating
- Prelude wrappers

Use for most projects.


## // minimal

Essential safety only. Enforces:

- No heredocs (almost always buggy)
- No `using namespace` in C++ headers

Use for legacy codebases or gradual adoption.


## // nixpkgs

nixpkgs contribution guidelines. Enforces:

- Required `meta` attribute with description
- No `with lib;` (warning)

Allows:

- `rec` (used in nixpkgs patterns)
- camelCase identifiers (nixpkgs convention)


## // security

Security-focused. Enforces:

- No heredocs (injection risk)
- No raw new/delete in C++ (memory safety)
- Text templating warnings (injection risk)


# ══════════════════════════════════════════════════════════════════════════════
#                                                               // custom config
# ══════════════════════════════════════════════════════════════════════════════

```dhall
let Narsil = ./config/package.dhall

let Severity = Narsil.Severity

in  Narsil.Config::{
    , profile = "standard"
    
    -- Ignore paths
    , extra-ignores = [
        "vendor/**"
      , "third-party/**"
      , "generated/**"
      ]
    
    -- Override specific rules
    , overrides = [
        -- Disable rec warnings entirely
        Narsil.override "rec-anywhere" Severity.Off
        
        -- Enable lisp-case as warning
      , Narsil.override "non-lisp-case" Severity.Warning
      
        -- Make missing-meta an error
      , Narsil.override "missing-meta" Severity.Error
      
        -- With a reason
      , Narsil.override-with-reason 
          "no-substitute-all" 
          Severity.Off 
          "We don't use Dhall yet"
      ]
    }
```


# ══════════════════════════════════════════════════════════════════════════════
#                                                                      // schema
# ══════════════════════════════════════════════════════════════════════════════


## // types.dhall

```dhall
let Severity = < Error | Warning | Info | Off >

let RuleOverride =
      { id : Text
      , severity : Severity
      , reason : Optional Text
      }

let Config =
      { profile : Text
      , extra-ignores : List Text
      , overrides : List RuleOverride
      }
```


## // package.dhall

Exports:

| Name | Type | Description |
|------|------|-------------|
| `Severity` | Type | `Error \| Warning \| Info \| Off` |
| `Config` | Type | Configuration record |
| `rule-ids` | Record | All rule IDs as text |
| `profiles` | Record | All profile definitions |
| `override` | Function | `Text → Severity → RuleOverride` |
| `override-with-reason` | Function | `Text → Severity → Text → RuleOverride` |
| `default-config` | Config | Standard profile, no overrides |


# ══════════════════════════════════════════════════════════════════════════════
#                                                                 // rule // ids
# ══════════════════════════════════════════════════════════════════════════════

Available rule IDs:

```dhall
-- nix policy
"rec-anywhere"
"with-lib"
"no-substitute-all"
"no-raw-mkderivation"
"no-raw-runcommand"
"no-raw-writeshellapplication"
"no-translate-attrs-outside-prelude"

-- nix best practices
"prefer-write-shell-application"
"no-heredoc-in-inline-bash"
"or-null-fallback"
"long-inline-string"

-- nix derivation quality
"missing-meta"
"missing-description"
"missing-class"

-- nix naming
"non-lisp-case"
"default-nix-in-packages"

-- cpp
"cpp-using-namespace-header"
"cpp-ban-cuda-identifier"
"cpp-raw-new-delete"
"cpp-namespace-closing-comment"
```

Access via `Narsil.rule-ids`:

```dhall
let R = Narsil.rule-ids

in [ Narsil.override R.rec-anywhere Severity.Off ]
```


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
#     "A year here and he still dreamed of cyberspace, hope fading nightly."
#
#                                                                  — Neuromancer
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                                 — b7r6 // 2026
