-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                          // rules
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "Case was twenty-four. At twenty-two, he'd been a cowboy."
--
--                                                                                     — Neuromancer
--

let Types = ./types.dhall

let Severity = Types.Severity

let Language = Types.Language

let Rule =
      { id : Text
      , language : Language
      , default-severity : Severity
      , description : Text
      , rationale : Text
      }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                   // nix // rules
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

let nix-rules =
      [ -- // policy // enforcement
        { id = "rec-anywhere"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Recursive attrsets outside prelude"
        , rationale = "Obscures dependency flow, hides evaluation order issues"
        }
      , { id = "with-lib"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "`with lib;` statement"
        , rationale = "Breaks tooling, creates shadowing hazards"
        }
      , { id = "no-substitute-all"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "substituteAll/replaceVars usage"
        , rationale = "Text templating must use Dhall"
        }
      , { id = "no-raw-mkderivation"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Direct mkDerivation call"
        , rationale = "Use prelude wrappers for typed paths"
        }
      , { id = "no-raw-runcommand"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Direct runCommand call"
        , rationale = "Use prelude wrappers"
        }
      , { id = "no-raw-writeshellapplication"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Direct writeShellApplication call"
        , rationale = "Use prelude wrappers"
        }
      , { id = "no-translate-attrs-outside-prelude"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Attribute translation outside prelude"
        , rationale = "Translation layer belongs in prelude only"
        }
      , -- // best // practices
        { id = "prefer-write-shell-application"
        , language = Language.Nix
        , default-severity = Severity.Warning
        , description = "writeShellScript instead of writeShellApplication"
        , rationale = "writeShellApplication has shellcheck, -euo pipefail"
        }
      , { id = "no-heredoc-in-inline-bash"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Heredoc in inline bash string"
        , rationale = "Fragile, hard to lint, agents produce buggy heredocs"
        }
      , { id = "or-null-fallback"
        , language = Language.Nix
        , default-severity = Severity.Warning
        , description = "Implicit null fallback pattern"
        , rationale = "Prefer explicit null handling"
        }
      , { id = "long-inline-string"
        , language = Language.Nix
        , default-severity = Severity.Warning
        , description = "Long inline string (>500 chars)"
        , rationale = "Move to external file for maintainability"
        }
      , -- // derivation // quality
        { id = "missing-meta"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Derivation without meta attribute"
        , rationale = "Metadata is required for discoverability"
        }
      , { id = "missing-description"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "meta without description"
        , rationale = "Description is essential documentation"
        }
      , { id = "missing-class"
        , language = Language.Nix
        , default-severity = Severity.Warning
        , description = "Package without class declaration"
        , rationale = "Classification helps organization"
        }
      , { id = "unused-binding"
        , language = Language.Nix
        , default-severity = Severity.Warning
        , description = "Let binding with no references"
        , rationale = "Dead code accumulates; the scope graph knows"
        }
      , -- // naming // conventions
        { id = "non-lisp-case"
        , language = Language.Nix
        , default-severity = Severity.Off
        , description = "Non-lisp-case identifier"
        , rationale = "Forces code through prelude (straylight-specific)"
        }
      , { id = "default-nix-in-packages"
        , language = Language.Nix
        , default-severity = Severity.Warning
        , description = "Package dir without default.nix"
        , rationale = "Consistent directory structure"
        }
      , { id = "type-check-failure"
        , language = Language.Nix
        , default-severity = Severity.Error
        , description = "Type inference failure treated as error"
        , rationale = "Type errors can indicate real bugs"
        }
      ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                   // cpp // rules
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

let cpp-rules =
      [ { id = "cpp-using-namespace-header"
        , language = Language.Cpp
        , default-severity = Severity.Error
        , description = "`using namespace` in header file"
        , rationale = "Pollutes namespace of all includers"
        }
      , { id = "cpp-ban-cuda-identifier"
        , language = Language.Cpp
        , default-severity = Severity.Warning
        , description = "Direct CUDA identifier usage"
        , rationale = "CUDA must go through abstraction layer"
        }
      , { id = "cpp-raw-new-delete"
        , language = Language.Cpp
        , default-severity = Severity.Error
        , description = "Raw new/delete usage"
        , rationale = "Use smart pointers"
        }
      , { id = "cpp-namespace-closing-comment"
        , language = Language.Cpp
        , default-severity = Severity.Info
        , description = "Missing namespace closing comment"
        , rationale = "Helps navigate large files"
        }
      ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                            // all
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

let all-rules = nix-rules # cpp-rules

let rule-ids =
      { -- nix policy
        rec-anywhere = "rec-anywhere"
      , with-lib = "with-lib"
      , no-substitute-all = "no-substitute-all"
      , no-raw-mkderivation = "no-raw-mkderivation"
      , no-raw-runcommand = "no-raw-runcommand"
      , no-raw-writeshellapplication = "no-raw-writeshellapplication"
      , no-translate-attrs-outside-prelude = "no-translate-attrs-outside-prelude"
      , -- nix best practices
        prefer-write-shell-application = "prefer-write-shell-application"
      , no-heredoc-in-inline-bash = "no-heredoc-in-inline-bash"
      , or-null-fallback = "or-null-fallback"
      , long-inline-string = "long-inline-string"
      , -- nix derivation quality
        missing-meta = "missing-meta"
      , missing-description = "missing-description"
      , missing-class = "missing-class"
      , unused-binding = "unused-binding"
      , -- nix naming
        non-lisp-case = "non-lisp-case"
      , default-nix-in-packages = "default-nix-in-packages"
      , type-check-failure = "type-check-failure"
      , -- cpp
        cpp-using-namespace-header = "cpp-using-namespace-header"
      , cpp-ban-cuda-identifier = "cpp-ban-cuda-identifier"
      , cpp-raw-new-delete = "cpp-raw-new-delete"
      , cpp-namespace-closing-comment = "cpp-namespace-closing-comment"
      }

in  { Rule, all-rules, nix-rules, cpp-rules, rule-ids }
