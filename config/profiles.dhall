-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                       // profiles
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "He'd found her, one rainy night, in an arcade."
--
--                                                                                     — Neuromancer
--

let Types = ./types.dhall

let Rules = ./rules.dhall

let Severity = Types.Severity

let RuleOverride = Types.RuleOverride

let Profile = Types.Profile

let R = Rules.rule-ids

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                         // strict
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--
-- Full straylight/aleph conventions.
-- Lisp-case identifiers, Dhall templating, prelude-only.
-- For new straylight projects or projects that want maximum rigor.

let strict
    : Profile
    = { name = "strict"
      , description = "Full aleph conventions with lisp-case enforcement"
      , parent = None Text
      , rules =
        [ -- everything error by default, enable lisp-case
          { id = R.non-lisp-case
          , severity = Severity.Error
          , reason = Some "Enforces prelude-only code paths"
          }
        , { id = R.no-substitute-all
          , severity = Severity.Error
          , reason = Some "All text templating via Dhall"
          }
        , { id = R.no-raw-mkderivation
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.no-raw-runcommand
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.no-raw-writeshellapplication
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.no-translate-attrs-outside-prelude
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.rec-anywhere
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.with-lib
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.no-heredoc-in-inline-bash
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.missing-meta
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.missing-description
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.cpp-using-namespace-header
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.cpp-raw-new-delete
          , severity = Severity.Error
          , reason = Some "Memory safety"
          }
        , { id = R.missing-class
          , severity = Severity.Error
          , reason = Some "Module compatibility must be explicit"
          }
        ]

      , ignores = [] : List Text
      , files = None (List Text)
      }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                       // standard
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--
-- Sensible defaults for most projects.
-- No lisp-case, no prelude requirements.
-- Catches real bugs and enforces best practices.

let standard
    : Profile
    = { name = "standard"
      , description = "Sensible defaults for most projects"
      , parent = None Text
      , rules =
        [ -- Turn OFF straylight-specific rules
          { id = R.non-lisp-case
          , severity = Severity.Off
          , reason = Some "Most projects use nixpkgs naming conventions"
          }
        , { id = R.no-substitute-all
          , severity = Severity.Off
          , reason = Some "Dhall templating is straylight-specific"
          }
        , { id = R.no-raw-mkderivation
          , severity = Severity.Off
          , reason = Some "Prelude wrappers are straylight-specific"
          }
        , { id = R.no-raw-runcommand
          , severity = Severity.Off
          , reason = Some "Prelude wrappers are straylight-specific"
          }
        , { id = R.no-raw-writeshellapplication
          , severity = Severity.Off
          , reason = Some "Prelude wrappers are straylight-specific"
          }
        , { id = R.no-translate-attrs-outside-prelude
          , severity = Severity.Off
          , reason = Some "Prelude is straylight-specific"
          }
        , -- Keep universally useful rules
          { id = R.rec-anywhere
          , severity = Severity.Warning
          , reason = Some "rec is sometimes legitimate"
          }
        , { id = R.with-lib
          , severity = Severity.Error
          , reason = Some "Universally bad practice"
          }
        , { id = R.no-heredoc-in-inline-bash
          , severity = Severity.Error
          , reason = Some "Fragile pattern, common source of bugs"
          }
        , { id = R.prefer-write-shell-application
          , severity = Severity.Warning
          , reason = None Text
          }
        , { id = R.missing-meta
          , severity = Severity.Warning
          , reason = None Text
          }
        , { id = R.missing-description
          , severity = Severity.Info
          , reason = None Text
          }
        , { id = R.missing-class
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.cpp-using-namespace-header
          , severity = Severity.Error
          , reason = None Text
          }
        , { id = R.cpp-raw-new-delete
          , severity = Severity.Warning
          , reason = None Text
          }
        ]
      , ignores = [] : List Text
      , files = None (List Text)
      }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // minimal
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--
-- Only the essential safety checks.
-- For legacy codebases or gradual adoption.

let minimal
    : Profile
    = { name = "minimal"
      , description = "Essential safety checks only"
      , parent = None Text
      , rules =
        [ -- Only the rules that catch real bugs
          { id = R.with-lib
          , severity = Severity.Warning
          , reason = Some "Common source of confusion"
          }
        , { id = R.no-heredoc-in-inline-bash
          , severity = Severity.Error
          , reason = Some "Almost always buggy"
          }
        , { id = R.missing-class
          , severity = Severity.Warning
          , reason = Some "Recommended for module safety"
          }
        , { id = R.cpp-using-namespace-header
          , severity = Severity.Error
          , reason = Some "Namespace pollution"
          }
        ]
      , ignores = [] : List Text
      , files = None (List Text)
      }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // nixpkgs
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--
-- For nixpkgs contributions.
-- Follows nixpkgs conventions.

let nixpkgs
    : Profile
    = { name = "nixpkgs"
      , description = "nixpkgs contribution guidelines"
      , parent = Some "standard"
      , rules =
        [ -- nixpkgs uses with and rec pervasively
          { id = R.with-lib
          , severity = Severity.Off
          , reason = Some "nixpkgs uses with pervasively as the primary scope mechanism"
          }
        , { id = R.rec-anywhere
          , severity = Severity.Off
          , reason = Some "nixpkgs uses rec in several legitimate patterns"
          }
        , -- nixpkgs-standard derivation patterns are expected
          { id = R.prefer-write-shell-application
          , severity = Severity.Off
          , reason = Some "both patterns are legitimate in nixpkgs"
          }
        , { id = R.or-null-fallback
          , severity = Severity.Off
          , reason = Some "or-null is a standard nixpkgs pattern"
          }
        , { id = R.long-inline-string
          , severity = Severity.Off
          , reason = Some "cosmetic; nixpkgs has its own formatting rules"
          }
        , -- Type inference is not precise enough for nixpkgs
          { id = R.type-check-failure
          , severity = Severity.Warning
          , reason =
              Some
                "lax while upstream PRs land: the residual stock-nixpkgs diagnostics are real bugs or ledgered exclusions (doc/design/contract.md)"
          }
        , -- Strong enforcement on derivation quality
          { id = R.missing-meta
          , severity = Severity.Error
          , reason = Some "Required by nixpkgs guidelines"
          }
        , { id = R.missing-description
          , severity = Severity.Error
          , reason = Some "Required by nixpkgs guidelines"
          }
        , { id = R.missing-class
          , severity = Severity.Error
          , reason = Some "Module compatibility must be explicit"
          }
        ]
      , ignores = [] : List Text
      , files = None (List Text)
      }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                       // security
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--
-- Security-focused checks.
-- For codebases where injection/memory safety is critical.

let security
    : Profile
    = { name = "security"
      , description = "Security-focused checks"
      , parent = Some "minimal"
      , rules =
        [ -- Heredocs can hide injection
          { id = R.no-heredoc-in-inline-bash
          , severity = Severity.Error
          , reason = Some "Potential injection vector"
          }
        , -- substituteAll can be injection vector
          { id = R.no-substitute-all
          , severity = Severity.Warning
          , reason = Some "Text interpolation can be injection vector"
          }
        , -- Memory safety
          { id = R.cpp-raw-new-delete
          , severity = Severity.Error
          , reason = Some "Memory safety"
          }
        ]
      , ignores = [] : List Text
      , files = None (List Text)
      }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                            // off
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--
-- All rules disabled. Use as base for custom profiles.

let off
    : Profile
    = { name = "off"
      , description = "All rules disabled"
      , parent = None Text
      , rules = [] : List RuleOverride
      , ignores = [ "**/*" ]
      , files = None (List Text)
      }

in  { strict, standard, minimal, nixpkgs, security, off }
